## Plot of displacement vs weight
png("disp_vs_wt.png")
plot(disp ~ wt, cars)
dev.off()

## Overall distribution of displacement
png("distribution.png")
hist(cars$disp, main = sprintf("(for %d cylinder cars)", cyl))
dev.off()
