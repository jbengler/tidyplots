#' @importFrom grDevices col2rgb rgb dev.off pdf
#' @importFrom stats density sd setNames
#' @importFrom utils tail
#' @importFrom rlang := .data
NULL

utils::globalVariables(c(".", ".single_color", "Lower", "Mean", "Upper", "aesthetic",
                         "col_zscore", "count", "row_zscore", "variable", "x", "y"))


color_palettes <- list(
  black_blue_pink_yellow = c(
    "#161523","#171943","#2C2E68","#2E3D88","#3F519D","#48569F","#5F569F",
    "#7D5099","#8B5197","#A14F96","#C85296","#DB5999","#E06A7F","#E27F6D",
    "#E68F63","#ECA562","#F3BD66","#F9D966","#FAF06F","#EEE969"),

  blue_pink_yellow = c(
    "#00034D","#000F9F","#001CEF","#241EF5","#5823F6","#A033E0","#E85AB1",
    "#F1907C","#F4AF63","#FCE552","#FFFB6D"),

  blue2brown = c(
    "#1961A5","#2671B5","#2D80BF","#268CC9","#119DD8","#00B2EB","#66C5EF",
    "#C4E5F8","#FEFCF6","#FDEEB8","#FCDD67","#F6C445","#E78B43","#DD5642",
    "#DB3E34","#CA3632","#B3322E"),

  bunq = c(
    "#248746","#2D9B47","#62B64F","#88CC53","#3CB7AA","#3296D7","#2772BA",
    "#1D5D83","#973431","#E23133","#F18823","#F5C836"),

  color_circle_vivid = c(
    "#939393","#644296","#F08533","#D1352C","#559E3F","#3B78B0"),

  ggsci_d3 = c(
    "#1F77B4","#FF7F0E","#2CA02C","#D62728","#9467BD","#8C564B","#E377C2",
    "#7F7F7F","#BCBD22","#17BECF"),

  ggsci_jco = c(
    "#0073C2","#EFC000","#868686","#CD534C","#7AA6DC","#003C67","#8F7700",
    "#3B3B3B","#A73030","#4A6990"),

  ggsci_locuszoom = c(
    "#D43F3A","#EEA236","#5CB85C","#46B8DA","#357EBD","#9632B8","#B8B8B8"),

  ggsci_npg = c(
    "#E64B35","#4DBBD5","#00A087","#3C5488","#F39B7F","#8491B4","#91D1C2",
    "#DC0000","#7E6148","#B09C85"),

  metro = c(
    "#4DACD6","#4FAE62","#F6C54D","#E37D46","#C02D45"),

  multiqc_align = c(
    "#437BB1","#7CB5EC","#F7A35C","#B1084C","#7F0000"),

  multiqc_count = c(
    "#7CB5EC","#434348","#90ED7D","#F7A35C"),

  multiqc_fastq_screen = c(
    "#87B5E7","#434348","#A6E98A","#ECA669","#8087E2","#E06681","#E2D269",
    "#F7A35C","#B1084C","#4A8E8E","#E36561","#77150C","#CCCCCC"),

  white_ocre_brown = c(
    "#FDFCF4","#F9F0CC","#F9E59F","#F4DA7A","#F1CB62","#EDBF55","#DF9350",
    "#D87D4A","#CF5C45","#BC403D","#AD3C39","#9F3736","#983534","#842E2E"),

  white_turquise_blue = c(
    "#FCFBDF","#F7F7D1","#EDF0BD","#E3EBB6","#CFE1B5","#C4DDB6","#9FCEB4",
    "#83C5B3","#5DBAB7","#1FB1BA","#00AFBF","#049FBF","#0493BF","#037DB5",
    "#036DAC","#16579E","#224B95","#2B3D8B","#2B3977","#232959","#1F2451")
)
