library(tidyverse)

expected_ve <- function(
    target_infection_unvac = 0.01,
    target_infection_vac_ve0 = 0.01,
    ve = 0.6,
    target_symptoms_unvac = 0.5,
    target_symptoms_vac = 0.5,
    nontarget_symptomatic_infection = 0.2,
    asympt_in_cases_vac = 0,
    asympt_in_cases_unvac = 0,
    healthy_in_noncases_unvac = 0,
    healthy_in_noncases_vac = 0,
    vaccine_coverage = 0.7,
    name = "name"
) {
    target_infection_vac <- target_infection_vac_ve0 * (1 - ve)

    healthy_vac <- 1 - target_infection_vac - nontarget_symptomatic_infection
    healthy_unvac <- 1 - target_infection_unvac - nontarget_symptomatic_infection

    asympt_vac <- 1 - target_symptoms_vac
    asympt_unvac <- 1 - target_symptoms_unvac

    vaccinated_cases <- vaccine_coverage * target_infection_vac * (target_symptoms_vac + asympt_vac * asympt_in_cases_vac)
    vaccinated_noncases <- vaccine_coverage * (nontarget_symptomatic_infection + healthy_vac * healthy_in_noncases_vac)

    unvac_cases <- (1 - vaccine_coverage) * target_infection_unvac * (target_symptoms_unvac + asympt_unvac * asympt_in_cases_unvac)
    unvac_noncases <- (1 - vaccine_coverage) * (nontarget_symptomatic_infection + healthy_unvac * healthy_in_noncases_unvac)

    odds_vaccination_cases <- vaccinated_cases / unvac_cases
    odds_vaccination_noncases <- vaccinated_noncases / unvac_noncases

    or <- odds_vaccination_cases / (odds_vaccination_noncases)
    est_ve <- 1 - or

    tibble(
        target_infection_unvac,
        target_infection_vac_ve0,
        true_ve = ve,
        target_symptoms_unvac,
        target_symptoms_vac,
        nontarget_symptomatic_infection,
        asympt_in_cases_vac,
        asympt_in_cases_unvac,
        healthy_in_noncases_unvac,
        healthy_in_noncases_vac,
        vaccine_coverage,
        name,
        target_infection_vac,
        healthy_vac,
        healthy_unvac,
        asympt_vac,
        asympt_unvac,
        vaccinated_cases,
        vaccinated_noncases,
        unvac_cases,
        unvac_noncases,
        odds_vaccination_cases,
        odds_vaccination_noncases,
        or,
        est_ve,
        addbias = est_ve - true_ve,
        mulbias = 1 - (1 - est_ve) / (1 - true_ve)
    )
}

npoints <- 300

args_asympt_in_cases_and_healthy_in_noncases_by_vac_status <- expand.grid(
    asympt_in_cases_vac = c(0, 0.1, 0.5),
    asympt_in_cases_unvac = c(0, 0.1, 0.5),
    healthy_in_noncases_vac = c(0, 0.1, 0.5),
    healthy_in_noncases_unvac = c(0, 0.1, 0.5)
)

res <- bind_rows(
    expected_ve(
        healthy_in_noncases_unvac = rep(seq(0, 1, length.out = npoints), each = npoints),
        healthy_in_noncases_vac = rep(seq(0, 1, length.out = npoints), times = npoints),
        name = "healthy_in_noncases"
    ),
    expected_ve(
        target_infection_vac_ve0 = rep(seq(0.005, 0.02, length.out = npoints), each = npoints), 
        target_infection_unvac = rep(seq(0.005, 0.02, length.out = npoints), times = npoints), 
        name = "target_infection"
    ),
    expected_ve(
        target_symptoms_vac = rep(seq(0.2, 0.8, length.out = npoints), each = npoints), 
        target_symptoms_unvac = rep(seq(0.2, 0.8, length.out = npoints), times = npoints), 
        name = "target_symptoms"
    ),
    expected_ve(
        asympt_in_cases_vac = rep(seq(0, 1, length.out = npoints), each = npoints), 
        asympt_in_cases_unvac = rep(seq(0, 1, length.out = npoints), times = npoints), 
        name = "asympt_in_cases"
    ),
    expected_ve(
        asympt_in_cases_vac = rep(seq(0, 1, length.out = npoints), each = npoints), 
        asympt_in_cases_unvac = rep(seq(0, 1, length.out = npoints), each = npoints), 
        healthy_in_noncases_vac = rep(seq(0, 1, length.out = npoints), times = npoints), 
        healthy_in_noncases_unvac = rep(seq(0, 1, length.out = npoints), times = npoints), 
        name = "asympt_in_cases_and_healthy_in_noncases"
    ),

    expected_ve(
        asympt_in_cases_vac = args_asympt_in_cases_and_healthy_in_noncases_by_vac_status$asympt_in_cases_vac,
        asympt_in_cases_unvac = args_asympt_in_cases_and_healthy_in_noncases_by_vac_status$asympt_in_cases_unvac,
        healthy_in_noncases_vac = args_asympt_in_cases_and_healthy_in_noncases_by_vac_status$healthy_in_noncases_vac,
        healthy_in_noncases_unvac = args_asympt_in_cases_and_healthy_in_noncases_by_vac_status$healthy_in_noncases_unvac,
        name = "asympt_in_cases_and_healthy_in_noncases_by_vac_status"
    )
)

write_csv(res, "res.csv")

genall <- function(biasname) {

    ggsave2 <- function(name, plot, ...) {
        ggsave(paste0(name, "_", biasname, ".pdf"), plot, units = "cm", ...)
        ggsave(paste0(name, "_", biasname, ".png"), plot, units = "cm", ...)
    }

    plot_comp <- list(
        theme_bw(),
        scale_fill_gradient2("Bias", labels = scales::percent_format(), limits = c(-1.5, 1)),
        scale_color_gradient2("Bias", labels = scales::percent_format(), limits = c(-1.5, 1)),
        geom_tile(aes(fill = !!sym(biasname), color = !!sym(biasname)))
    )

    plot_healthy_in_noncases <- res %>%
        filter(name == "healthy_in_noncases") %>%
        ggplot(aes(healthy_in_noncases_vac, healthy_in_noncases_unvac)) +
        plot_comp +
        scale_x_continuous("Proportion healthy in noncases in vaccinated", expand = expansion(0, 0), labels = scales::percent_format()) +
        scale_y_continuous("Proportion healthy in noncases in unvaccinated", expand = expansion(0, 0), labels = scales::percent_format())

    width <- 13
    height <- 10

    ggsave2("healthy_in_noncases", plot_healthy_in_noncases, width = width, height = height)

    plot_target_infection <- res %>%
        filter(name == "target_infection") %>%
        ggplot(aes(target_infection_vac_ve0, target_infection_unvac)) +
        plot_comp +
        scale_x_continuous("Risk of target infection in vaccinated", expand = expansion(0, 0), labels = scales::percent_format()) +
        scale_y_continuous("Risk of target infection in unvaccinated", expand = expansion(0, 0), labels = scales::percent_format())

    ggsave2("target_infection", plot_target_infection, width = width, height = height)

    plot_target_symptoms <- res %>%
        filter(name == "target_symptoms") %>%
        ggplot(aes(target_symptoms_vac, target_symptoms_unvac)) +
        plot_comp +
        scale_x_continuous("Proprotion of symptomatic infections in vaccinated", expand = expansion(0, 0), labels = scales::percent_format()) +
        scale_y_continuous("Proprotion of symptomatic infections in unvaccinated", expand = expansion(0, 0), labels = scales::percent_format())

    ggsave2("target_symptoms", plot_target_symptoms, width = width, height = height)

    plot_asympt_in_cases <- res %>%
        filter(name == "asympt_in_cases") %>%
        ggplot(aes(asympt_in_cases_vac, asympt_in_cases_unvac)) +
        plot_comp +
        scale_x_continuous("Proportion asymptomatic in cases in vaccinated", expand = expansion(0, 0), labels = scales::percent_format()) +
        scale_y_continuous("Proportion asymptomatic in cases in unvaccinated", expand = expansion(0, 0), labels = scales::percent_format())

    ggsave2("asympt_in_cases", plot_asympt_in_cases, width = width, height = height)

    plot_asympt_in_cases_and_healthy_in_noncases <- res %>%
        filter(name == "asympt_in_cases_and_healthy_in_noncases") %>%
        ggplot(aes(asympt_in_cases_vac, healthy_in_noncases_vac)) +
        theme_bw() +
        scale_fill_gradient2("Bias", labels = scales::percent_format()) +
        scale_color_gradient2("Bias", labels = scales::percent_format()) +
        geom_tile(aes(fill = !!sym(biasname), color = !!sym(biasname))) +
        scale_x_continuous("Proportion asymptomatic in cases", expand = expansion(0, 0), labels = scales::percent_format()) +
        scale_y_continuous("Proportion healthy in noncases", expand = expansion(0, 0), labels = scales::percent_format())

    ggsave2("asympt_in_cases_and_healthy_in_noncases", plot_asympt_in_cases_and_healthy_in_noncases, width = width, height = height)

    plot_asympt_in_cases_and_healthy_in_noncases_by_vac_status <- res %>%
        filter(name == "asympt_in_cases_and_healthy_in_noncases_by_vac_status") %>%
        mutate(
            asympt_in_cases = glue::glue("{asympt_in_cases_vac * 100}%V {asympt_in_cases_unvac * 100}%UV") %>% fct_reorder(asympt_in_cases_unvac) %>% fct_reorder(asympt_in_cases_vac),
            healthy_in_noncases = glue::glue("{healthy_in_noncases_vac * 100}%V {healthy_in_noncases_unvac * 100}%UV") %>% fct_reorder(healthy_in_noncases_unvac) %>% fct_reorder(healthy_in_noncases_vac),
        ) %>%
        select(!!sym(biasname), asympt_in_cases, healthy_in_noncases) %>%
        ggplot(aes(healthy_in_noncases, asympt_in_cases)) +
        plot_comp +
        geom_label(aes(label = round(!!sym(biasname) * 100) %>% paste0("%")), alpha = 0.5, label.size = 0) +
        theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
        scale_x_discrete("Proportion healthy in noncases", expand = expansion(0, 0)) +
        scale_y_discrete("Proportion asymptomatic in cases", expand = expansion(0, 0))

    ggsave2("asympt_in_cases_and_healthy_in_noncases_by_vac_status", plot_asympt_in_cases_and_healthy_in_noncases_by_vac_status, width = width * 1.5, height = height * 1.5)

    combined <- ggpubr::ggarrange(
        ggpubr::ggarrange(
            plot_healthy_in_noncases + theme(legend.position = "none", plot.margin = margin(10, 15, 10, 10)) + ggtitle("a"),
            plot_asympt_in_cases + theme(legend.position = "none", plot.margin = margin(10, 15, 10, 10)) + ggtitle("b")
        ),
        plot_asympt_in_cases_and_healthy_in_noncases_by_vac_status + ggtitle("c") + theme(legend.title = element_blank()),
        ncol = 1,
        heights = c(1, 1.5)
    )    

    ggsave2("combined", combined, width = 20, height = 25)
}

genall("addbias")
genall("mulbias")
