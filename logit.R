logit1 <- lm(data = data, formula = ls_ls0 ~ p_adj + dpm + door3 + door4 + door5 + at + ps + air + drv + wt + hp2wt + hp + euro + japan + size + wb )
summary(logit1)

logit2 <- lm(data = data, formula = ls_ls0 ~ p_adj + dpm + door3 + door4 + door5 + at + ps + air + drv + wt + hp2wt + hp + euro + japan + size + wb
             + firm15 + firm20 + firm19 + firm8 + firm16 + firm2 + firm18 + firm17 + firm3 + firm9 + firm4 + firm14 + firm12 + firm24 + firm11 + firm5 + firm7)
summary(logit2)