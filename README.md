# Brexit Referendum Data Analysis

This project performs an in-depth statistical analysis of the 2016 Brexit referendum results across different electoral wards in the UK. Using advanced machine learning techniques, we explore the demographic factors that might have influenced the voting patterns.

## 📊 Project Overview
This project applies various statistical and machine learning techniques to identify key factors that influenced the Brexit vote at a ward level.

### 🎯 Key Objectives
- Identify demographic clusters in voting patterns using K-means clustering
- Analyze the relationship between socio-economic factors and Brexit votes
- Validate findings against The Guardian's initial demographic analysis
- Provide robust statistical insights using logistic regression and BAGGING

### 📋 Dataset Features
The analysis uses normalized data (0-1 scale) with the following variables:
- **ABC1**: Proportion of middle to upper social classes
- **Median Income**: Residents' median income
- **Median Age**: Residents' median age
- **Higher Education**: Proportion with university education
- **Non-UK Born**: Proportion of residents born outside the UK
- **Vote Brexit**: Binary outcome (TRUE/FALSE for >50% Leave vote)

## 🔍 Analysis Approach

### Clustering Analysis
- K-means clustering to identify demographic patterns
- Cluster validation and optimization
- Visual cluster analysis

### Logistic Regression
- Full model coefficient analysis
- Effect size interpretation
- Variable importance ranking

### Robust Analysis
- BAGGING implementation
- Coefficient stability analysis
- Alternative regression approaches

## 🛠️ Getting Started

### Prerequisites
Required R packages:
```r
install.packages(c("stats", "ggplot2", "caret", "dplyr"))
```

Data Loading:
Load the Brexit dataset

```r
brexit_data <- read.csv("brexit.csv")
```





## 📈 Key Findings
Our analysis revealed several interesting patterns:
- Demographic clustering showed distinct voter groups
- Socio-economic factors had varying degrees of influence
- Results validated several of The Guardian's initial observations
- BAGGING analysis provided robust variable importance rankings

## 🤝 Contributing
Feel free to contribute to this analysis:
1. Fork the repository
2. Create your feature branch
3. Submit a pull request

## 📚 References
- Original data source: The Guardian's EU referendum analysis


**Note:** This analysis is based on electoral ward-level data and should be interpreted within its statistical context.
