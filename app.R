# ============================================================
# Expert Heart Disease Prediction — Shiny Chatbot
# Converted from original Expert_heart_disease_prediction app
# ============================================================

library(shiny)
library(shinydashboard)
library(shinythemes)
library(caTools)
library(randomForest)
library(ggplot2)
library(dplyr)

# ===================== DATA PREPARATION =====================

simulated_data <- function(n, y) {
  y <- ifelse((y == "X0") | (y == 0), 0, 1)
  Risk_Level1 <- ifelse(y == 1,
    sample(c(2,1,3), n, replace=TRUE, prob=c(0.3,0.6,0.1)),
    sample(c(3,2,1), n, replace=TRUE, prob=c(0.6,0.3,0.1)))
  Smoking_Status1 <- ifelse(y == 1,
    sample(c(2,3,1), n, replace=TRUE, prob=c(0.4,0.5,0.1)),
    sample(c(1,2,3), n, replace=TRUE, prob=c(0.5,0.4,0.1)))
  data.frame(
    Risk_Level = Risk_Level1,
    Healthy_Diet_Score = round(runif(n,7,10),1),
    Regular_Physical_Activity = sample(c(2,1), n, replace=TRUE, prob=c(0.7,0.3)),
    Stress_Management_Score = round(runif(n,5,10),1),
    Sleep_Hours = round(runif(n,6,9),1),
    Hydration_Liters = round(runif(n,1.5,3),1),
    Low_Impact_Exercise_Days = sample(0:7, n, replace=TRUE,
      prob=c(0.1,0.1,0.15,0.15,0.2,0.15,0.1,0.05)),
    Fiber_Intake_grams = round(runif(n,20,35),1),
    Omega3_Intake_mg = round(runif(n,500,2000),1),
    Weight_Management_Success = sample(c(1,2,3), n, replace=TRUE, prob=c(0.5,0.25,0.25)),
    BP_Monitoring_Days = sample(0:7, n, replace=TRUE,
      prob=c(0.2,0.2,0.15,0.15,0.1,0.1,0.05,0.05)),
    Alcohol_Units_per_Week = round(runif(n,0,14),1),
    Targeted_Nutritional_Score = round(runif(n,5,10),1),
    Supervised_Activity_Minutes = round(runif(n,0,60),1),
    Smoking_Status = Smoking_Status1,
    Symptom_Log_Days = sample(0:7, n, replace=TRUE,
      prob=c(0.1,0.1,0.2,0.2,0.2,0.1,0.05,0.05)),
    Emergency_Preparedness = sample(c(2,1), n, replace=TRUE, prob=c(0.8,0.2))
  )
}

d <- read.table("processed_cleveland.csv", sep=",", header=FALSE,
  col.names=c("age","sex","cp","trestbps","chol","fbs","restecg",
              "thalach","exang","oldpeak","slope","ca","thal","target"))
d$ca   <- as.integer(d$ca)
d$thal <- as.integer(d$thal)
d[d == "?"] <- NA
d <- transform(d,
  age=as.integer(age), sex=as.factor(sex), cp=as.factor(cp),
  trestbps=as.integer(trestbps), chol=as.integer(chol), fbs=as.factor(fbs),
  restecg=as.factor(restecg), thalach=as.integer(thalach), exang=as.factor(exang),
  oldpeak=as.numeric(oldpeak), slope=as.factor(slope), ca=as.factor(ca),
  thal=as.factor(thal), target=as.factor(target))
d <- d[!(d$ca %in% c(NA)),]
d <- d[!(d$thalach %in% c(NA)),]
d[,"target"] <- ifelse(d[,"target"]==0, 0, 1)
d[,"target"] <- as.factor(d[,"target"])
levels(d$target) <- make.names(levels(factor(d$target)))
d$target <- relevel(d$target, "X1")
names(d)[names(d)=="target"] <- "y"
d <- na.omit(d)
set.seed(12)
d <- data.frame(d, simulated_data(n=297, y=d$y))
d$Risk_Level <- as.factor(d$Risk_Level)
d$Regular_Physical_Activity <- as.factor(d$Regular_Physical_Activity)
d$Weight_Management_Success <- as.factor(d$Weight_Management_Success)
d$Smoking_Status <- as.factor(d$Smoking_Status)
d$Emergency_Preparedness <- as.factor(d$Emergency_Preparedness)

sample_split <- sample.split(d$y, SplitRatio=0.70)
train <- subset(d, sample_split==TRUE)
set.seed(12)
myModel1 <- randomForest(y ~ ., data=train)

# EDA dataset
df <- read.csv("processed.cleveland.data")
names(df) <- c("age","sex","cp","trestbps","chol","fbs","restecg",
               "thalach","exang","oldpeak","slope","ca","thal","target")
set.seed(12)
df <- data.frame(df, simulated_data(n=302, y=df$target))
df$Risk_Level <- as.factor(df$Risk_Level)
df$Regular_Physical_Activity <- as.factor(df$Regular_Physical_Activity)
df$Weight_Management_Success <- as.factor(df$Weight_Management_Success)
df$Smoking_Status <- as.factor(df$Smoking_Status)
df$Emergency_Preparedness <- as.factor(df$Emergency_Preparedness)
df$target <- ifelse(df$target > 0, 1, 0)
df$heart_disease <- ifelse(df$target > 0, "Yes", "No")
df$fbs_range     <- ifelse(df$fbs > 0, "> 120", "< 120")
df$Gender        <- ifelse(df$sex > 0, "Male", "Female")
df$exercise_angina <- ifelse(df$exang > 0, "Yes", "No")
df$slope_segment <- dplyr::case_when(
  df$slope==1 ~ "Upslope", df$slope==2 ~ "Flat", TRUE ~ "Downslope")
df$restecg_report <- dplyr::case_when(
  df$restecg==0 ~ "Normal",
  df$restecg==1 ~ "ST-T wave abnormality",
  TRUE ~ "Probable or definite left ventricular hypertrophy")
df$Chest_Pain_type <- dplyr::case_when(
  df$cp==1 ~ "typical angina", df$cp==2 ~ "atypical angina",
  df$cp==3 ~ "non-anginal pain", TRUE ~ "asymptomatic")
df$thal_report <- dplyr::case_when(
  df$thal=="3.0" ~ "Normal", df$thal=="6.0" ~ "Fixed Defect",
  TRUE ~ "Reversible Defect")
df$ca_number <- as.numeric(sub("\\.0","",df$ca))
df$Risk_Level_report <- dplyr::case_when(
  df$Risk_Level==1 ~ "High", df$Risk_Level==2 ~ "Medium", TRUE ~ "Mild")
df$Smoking_Status_report <- dplyr::case_when(
  df$Smoking_Status==1 ~ "Non-smoker", df$Smoking_Status==2 ~ "Former smoker",
  TRUE ~ "Current smoker")

# ===================== CHATBOT LOGIC =====================

chatbot_response <- function(msg, pred_prob = NULL, pred_done = FALSE) {
  m <- tolower(trimws(msg))

  # Greeting
  if (grepl("^(hi|hello|hey|good morning|good afternoon)", m)) {
    return("👋 Hello! I'm your **Heart Health Assistant**. I can:\n\n- Explain what each input means\n- Help interpret your prediction result\n- Answer questions about heart disease risk factors\n- Guide you through the form\n\nType *\"help\"* for a full list of topics!")
  }

  # Help
  if (grepl("help|what can you do|topics|options|commands", m)) {
    return("**I can help you with:**\n\n❓ **Inputs** – Ask what any field means (e.g., *\"what is oldpeak?\"*, *\"explain thalach\"*)\n\n💡 **Risk factors** – Ask about cholesterol, blood pressure, smoking etc.\n\n📊 **Prediction** – After clicking **Predict**, ask *\"what does my result mean?\"*\n\n🏥 **Model** – Ask how the Random Forest model works\n\n🌿 **Lifestyle** – Ask about diet, exercise, sleep impact on heart health")
  }

  # ---- Clinical input explanations ----
  if (grepl("oldpeak|st depression|st segment", m)) {
    return("📈 **Oldpeak** is the ST depression induced by exercise relative to rest on an ECG. Higher values (>2.0) may indicate ischaemia. A value of 0 is normal. It's one of the strongest predictors in this model.")
  }
  if (grepl("thalach|maximum heart rate|max heart rate|max.*hr", m)) {
    return("💓 **Thalach** is your **maximum heart rate achieved** during exercise testing. Healthy individuals typically reach higher max heart rates. Lower values relative to age can indicate cardiac stress. Formula: estimated max HR = 220 − age.")
  }
  if (grepl("trestbps|resting blood pressure|blood pressure|bp", m)) {
    return("🩸 **Trestbps** is your **resting blood pressure** (mmHg) recorded on hospital admission. Normal: <120/80. Elevated: 120–129 systolic. Hypertension Stage 1: 130–139. Stage 2: ≥140. High BP is a major heart disease risk factor.")
  }
  if (grepl("chol|cholesterol", m)) {
    return("🧪 **Chol** is your **serum cholesterol** in mg/dL. Desirable: <200 mg/dL. Borderline high: 200–239. High: ≥240. High LDL cholesterol is a leading cause of arterial plaque build-up.")
  }
  if (grepl("\\bfbs\\b|fasting blood sugar", m)) {
    return("🍬 **FBS** is **Fasting Blood Sugar**. A value >120 mg/dL is coded as 1 (Yes). Elevated fasting blood sugar suggests pre-diabetes or diabetes, both significant heart disease risk factors.")
  }
  if (grepl("restecg|electrocardiograph|ecg|ekg", m)) {
    return("📟 **RestECG** is the resting electrocardiographic result:\n- **0** = Normal\n- **1** = ST-T wave abnormality (possible ischaemia)\n- **2** = Left ventricular hypertrophy (heart wall thickening)\n\nAbnormal ECG results are associated with higher heart disease risk.")
  }
  if (grepl("exang|exercise.*angina|angina.*exercise", m)) {
    return("🏃 **Exercise Angina** (exang) indicates whether you experience chest pain (angina) during exercise. If yes (1), this strongly suggests reduced blood flow to the heart muscle during physical stress.")
  }
  if (grepl("\\bslope\\b|st.*slope|peak.*exercise.*st", m)) {
    return("📉 **Slope** of the peak exercise ST segment:\n- **1** = Upsloping (generally better)\n- **2** = Flat (borderline)\n- **3** = Downsloping (higher concern)\n\nDownsloping ST depression is more closely associated with ischaemia.")
  }
  if (grepl("\\bca\\b|vessels|flouroscopy|major vessel", m)) {
    return("🫀 **CA** is the number of major coronary vessels (0–3) coloured by fluoroscopy. More vessels showing calcification (higher CA) corresponds to greater coronary artery disease severity.")
  }
  if (grepl("thal|thalassemia|thalassaemia|thal report", m)) {
    return("🔬 **Thal** refers to thalassaemia status on a nuclear stress test:\n- **3** = Normal blood flow\n- **6** = Fixed defect (permanently reduced flow)\n- **7** = Reversible defect (flow reduced during stress but normal at rest)\n\nReversible defects are strongly associated with coronary artery disease.")
  }
  if (grepl("\\bcp\\b|chest pain type|angina type", m)) {
    return("💢 **Chest Pain Type (CP):**\n- **1** = Typical angina (classic: pressure, exertion-related)\n- **2** = Atypical angina (not all classic features)\n- **3** = Non-anginal pain (chest pain unlikely cardiac)\n- **4** = Asymptomatic (no chest pain)\n\nInterestingly, asymptomatic patients in this dataset sometimes have higher disease rates — silent ischaemia is common.")
  }
  if (grepl("age|how old|older", m)) {
    return("🎂 **Age** is a primary non-modifiable risk factor. Cardiovascular disease risk increases significantly after age 45 for men and 55 for women. In this dataset, the average age is around 54.")
  }
  if (grepl("sex|gender|male|female", m)) {
    return("⚧ **Sex/Gender**: Males have higher heart disease risk at younger ages, but after menopause, women's risk increases significantly. In this dataset, males constitute ~68% of cases.")
  }

  # ---- Lifestyle inputs ----
  if (grepl("smoking|smoke|cigarette|tobacco", m)) {
    return("🚬 **Smoking** is one of the biggest modifiable risk factors. Current smokers have ~2–4× higher risk of coronary artery disease. Former smokers see risk reduction within 1–2 years of quitting. In this app, smoking is classified as Non-smoker / Former smoker / Current smoker.")
  }
  if (grepl("diet|food|eating|nutrition|fiber|fibre|omega", m)) {
    return("🥗 **Diet** plays a crucial role in heart health:\n- High fibre (>25g/day) reduces LDL cholesterol\n- Omega-3 fatty acids (fish, walnuts) lower triglycerides and reduce inflammation\n- Mediterranean-style diets are associated with ~30% lower cardiovascular events\n- A Healthy Diet Score ≥7 is used in this model as a positive indicator")
  }
  if (grepl("exercise|physical activity|active|fitness|workout", m)) {
    return("🏋️ **Physical Activity** is one of the most powerful interventions:\n- 150 min/week of moderate exercise reduces heart disease risk by ~35%\n- Even low-impact exercise (walking, swimming) offers significant benefits\n- This model tracks both regular activity and supervised activity minutes")
  }
  if (grepl("sleep|rest|insomnia", m)) {
    return("😴 **Sleep** directly affects cardiovascular health:\n- <6 hours/night is associated with 20–30% higher heart disease risk\n- Optimal sleep: 7–9 hours/night\n- Poor sleep increases blood pressure, inflammation, and diabetes risk")
  }
  if (grepl("stress|anxiety|mental health|cortisol", m)) {
    return("🧠 **Stress Management** matters: chronic stress raises cortisol, increases blood pressure and heart rate, promotes inflammation, and encourages unhealthy coping behaviours (smoking, poor diet). A Stress Management Score ≥7 is considered protective in this model.")
  }
  if (grepl("alcohol|drink|unit", m)) {
    return("🍷 **Alcohol**: Moderate intake (≤14 units/week) has mixed evidence. Heavy drinking (>14 units/week) raises blood pressure, damages heart muscle, and increases arrhythmia risk. UK guidelines recommend ≤14 units/week spread over several days.")
  }
  if (grepl("hydration|water|drink.*water|fluid", m)) {
    return("💧 **Hydration**: Adequate daily fluid intake (1.5–3L/day) supports healthy blood viscosity and kidney function, both linked to blood pressure regulation. Dehydration can cause heart rate increases and circulatory stress.")
  }
  if (grepl("risk level|cardiovascular risk|risk.*category", m)) {
    return("⚠️ **Cardiovascular Risk Level** used in this model:\n- **Mild (3)** – Lower overall risk profile\n- **Medium (2)** – Moderate risk indicators present\n- **High (1)** – Multiple significant risk factors\n\nThis composite risk level was simulated based on the clinical features and is used as an additional predictor.")
  }

  # ---- Prediction result interpretation ----
  if (grepl("result|prediction|predict|probability|what does.*mean|interpret|score|risk", m)) {
    if (pred_done && !is.null(pred_prob)) {
      pct <- round(pred_prob * 100, 1)
      level <- if (pred_prob > 0.7) "⚠️ High"
               else if (pred_prob > 0.4) "🔶 Moderate"
               else "✅ Lower"
      advice <- if (pred_prob > 0.5)
        "**Please consult a healthcare professional promptly.** Meanwhile, focus on:\n- Reducing dietary fat and salt\n- Stopping smoking if applicable\n- Daily moderate exercise\n- Monitoring blood pressure regularly"
      else
        "**Keep up your healthy habits!** Continue:\n- Regular exercise (150+ min/week)\n- A balanced, fibre-rich diet\n- Good sleep hygiene\n- Regular check-ups"
      return(paste0("🫀 **Your Prediction Result**\n\nProbability of heart disease: **", pct, "%**\nRisk category: **", level, " risk**\n\n", advice))
    } else {
      return("Fill in all the patient details in the **Prediction** tab and click **Predict Heart Disease Status** to get your result. Then ask me to interpret it!")
    }
  }

  # ---- Model / technical ----
  if (grepl("random forest|model|algorithm|machine learning|how.*work|accuracy", m)) {
    return("🌲 **Random Forest** is an ensemble of decision trees:\n\n1. Multiple trees are trained on random subsets of the training data\n2. Each tree makes its own prediction\n3. The final prediction is the **majority vote** across all trees\n\nAdvantages: handles non-linear relationships, resistant to overfitting, works well with mixed data types. This model was trained on the **Cleveland Heart Disease dataset** (303 patients) augmented with simulated lifestyle features.")
  }
  if (grepl("dataset|cleveland|data|patients|sample size", m)) {
    return("📋 **Dataset**: The Cleveland Heart Disease dataset from the UCI Machine Learning Repository, containing **303 patients** with 14 clinical features. The target variable is presence (1) or absence (0) of heart disease. The dataset was augmented with 16 simulated lifestyle variables to enrich the model.")
  }

  # About
  if (grepl("about|developer|author|credit|who made", m)) {
    return("👨‍💻 This chatbot app was converted from the original **Expert Heart Disease Prediction** Shiny app. The original app used a **Random Forest** classifier trained on the UCI Cleveland Heart Disease dataset, combined with simulated lifestyle risk data.\n\nThe chatbot interface adds conversational guidance to help users understand their inputs and results.")
  }

  # Fallback
  return("🤔 I didn't quite catch that. Try asking:\n\n- *\"What is oldpeak?\"* / *\"Explain thalach\"*\n- *\"What does my result mean?\"*\n- *\"How does Random Forest work?\"*\n- *\"Tips for reducing heart disease risk\"*\n- *\"What is cholesterol?\"*\n\nOr type *\"help\"* for all topics.")
}


# ===================== UI =====================

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "💓 Heart Disease Prediction"),

  dashboardSidebar(
    sidebarMenu(
      menuItem("📊 EDA — Demographics",  tabName = "eda1", icon = icon("chart-bar")),
      menuItem("📈 EDA — Clinical",      tabName = "eda2", icon = icon("chart-line")),
      menuItem("🏃 EDA — Lifestyle",     tabName = "eda3", icon = icon("heart")),
      menuItem("🩺 Prediction & Chat",   tabName = "pred", icon = icon("stethoscope"))
    )
  ),

  dashboardBody(
    tags$head(tags$style(HTML("
      .chat-container {
        height: 360px; overflow-y: auto;
        background: #fff; border: 1px solid #dde;
        border-radius: 8px; padding: 12px;
        display: flex; flex-direction: column; gap: 8px;
      }
      .msg-user {
        align-self: flex-end; background: #2c3e50; color:#fff;
        border-radius: 16px 16px 4px 16px;
        padding: 8px 14px; max-width: 80%; font-size: 0.91em;
      }
      .msg-bot {
        align-self: flex-start; background: #ecf0f1; color:#2c3e50;
        border-radius: 16px 16px 16px 4px;
        padding: 8px 14px; max-width: 82%; font-size: 0.91em;
      }
      .chat-input-row { display:flex; gap:8px; margin-top:8px; }
      .chat-input-row input { flex:1; }
      .pred-result { font-size: 1.1em; font-weight: bold; margin: 10px 0; }
    "))),

    tabItems(

      # ---- EDA Tab 1: Demographics ----
      tabItem(tabName = "eda1",
        h3("EDA — Demographics & Blood Pressure"),
        fluidRow(
          box(title = "Age Distribution by Heart Disease",
              plotOutput("plot_age", height="260px"),
              numericInput("filter_age","Max age filter:", 70, min=30, max=80)),
          box(title = "Gender vs Heart Disease",
              plotOutput("plot_gender", height="260px"),
              checkboxGroupInput("filter_gender","Gender:",
                c("Male"="Male","Female"="Female"),
                selected=c("Male","Female")))
        ),
        fluidRow(
          box(title = "Resting BP vs Heart Disease",
              plotOutput("plot_bp", height="260px"),
              sliderInput("filter_bp","Max BP:", min(df$trestbps), max(df$trestbps), 160)),
          box(title = "Chest Pain Type vs Heart Disease",
              plotOutput("plot_cp", height="260px"),
              checkboxGroupInput("filter_cp","Chest Pain:",
                c("typical angina","atypical angina","non-anginal pain","asymptomatic"),
                selected=c("typical angina","atypical angina","non-anginal pain","asymptomatic")))
        )
      ),

      # ---- EDA Tab 2: Clinical ----
      tabItem(tabName = "eda2",
        h3("EDA — Clinical Indicators"),
        fluidRow(
          box(title = "Cholesterol Distribution",
              plotOutput("plot_chol", height="260px"),
              sliderInput("filter_chol","Max Chol:", min(df$chol), max(df$chol), 300)),
          box(title = "Max Heart Rate (Thalach)",
              plotOutput("plot_thalach", height="260px"),
              numericInput("filter_thalach","Max Thalach:", 150, min=70, max=210))
        ),
        fluidRow(
          box(title = "Exercise Angina vs Heart Disease",
              plotOutput("plot_exang", height="260px"),
              checkboxGroupInput("filter_exang","Exercise Angina:",
                c("Yes","No"), selected=c("Yes","No"))),
          box(title = "Thal Report vs Heart Disease",
              plotOutput("plot_thal", height="260px"),
              checkboxGroupInput("filter_thal","Thal:",
                c("Normal","Fixed Defect","Reversible Defect"),
                selected=c("Normal","Fixed Defect","Reversible Defect")))
        )
      ),

      # ---- EDA Tab 3: Lifestyle ----
      tabItem(tabName = "eda3",
        h3("EDA — Lifestyle Factors"),
        fluidRow(
          box(title = "Risk Level Distribution",
              plotOutput("plot_risk", height="260px"),
              checkboxGroupInput("filter_risk","Risk Level:",
                c("Mild","Medium","High"), selected=c("Mild","Medium","High"))),
          box(title = "Smoking Status vs Heart Disease",
              plotOutput("plot_smoke", height="260px"),
              checkboxGroupInput("filter_smoke","Smoking:",
                c("Non-smoker","Former smoker","Current smoker"),
                selected=c("Non-smoker","Former smoker","Current smoker")))
        ),
        fluidRow(
          box(title = "Diet Score vs Hydration",
              plotOutput("plot_diet", height="260px")),
          box(title = "Sleep vs Stress Management",
              plotOutput("plot_sleep", height="260px"))
        )
      ),

      # ---- Prediction + Chat ----
      tabItem(tabName = "pred",
        h3("🩺 Prediction & Chat Assistant"),
        fluidRow(
          # Left: inputs
          column(5,
            box(width=12, title="Patient Details", status="primary", solidHeader=TRUE,
              fluidRow(
                column(6, numericInput("age","Age:", 55, 20, 90)),
                column(6, radioButtons("sex","Sex:", c("Male"=1,"Female"=0), selected=1, inline=TRUE))
              ),
              radioButtons("cp","Chest Pain Type:",
                c("Typical angina"=1,"Atypical angina"=2,"Non-anginal"=3,"Asymptomatic"=4),
                selected=4, inline=TRUE),
              fluidRow(
                column(6, numericInput("trestbps","Resting BP (mmHg):", 130, 80, 220)),
                column(6, numericInput("chol","Cholesterol (mg/dL):", 250, 100, 600))
              ),
              fluidRow(
                column(6, radioButtons("fbs","FBS > 120?", c("No"=0,"Yes"=1), selected=0, inline=TRUE)),
                column(6, radioButtons("restecg","Rest ECG:",
                  c("Normal"=0,"ST-T abnorm."=1,"LV hypertrophy"=2), selected=0, inline=TRUE))
              ),
              fluidRow(
                column(6, numericInput("thalach","Max Heart Rate:", 150, 60, 220)),
                column(6, radioButtons("exang","Exercise Angina:", c("No"=0,"Yes"=1), selected=0, inline=TRUE))
              ),
              fluidRow(
                column(4, numericInput("oldpeak","Oldpeak:", 1.0, 0, 7, step=0.1)),
                column(4, radioButtons("slope","Slope:", c("Up"=1,"Flat"=2,"Down"=3), selected=2, inline=TRUE)),
                column(4, radioButtons("ca","CA (0-3):", c("0"=0,"1"=1,"2"=2,"3"=3), selected=0, inline=TRUE))
              ),
              radioButtons("thal","Thal:", c("Normal"=3,"Fixed def."=6,"Reversible def."=7), selected=3, inline=TRUE),
              hr(),
              h5("Lifestyle Factors"),
              fluidRow(
                column(6,
                  radioButtons("Risk_Level","Risk Level:", c("Mild"=3,"Medium"=2,"High"=1), selected=3, inline=TRUE),
                  radioButtons("Regular_Physical_Activity","Regular Exercise?", c("Yes"=2,"No"=1), selected=2, inline=TRUE),
                  radioButtons("Smoking_Status","Smoking:", c("Non-smoker"=1,"Former"=2,"Current"=3), selected=1, inline=TRUE)
                ),
                column(6,
                  numericInput("Healthy_Diet_Score","Diet Score (1-10):", 7.5, 1, 10, step=0.5),
                  numericInput("Sleep_Hours","Sleep (hrs/night):", 7, 4, 12, step=0.5),
                  numericInput("Alcohol_Units_per_Week","Alcohol (units/wk):", 5, 0, 50)
                )
              ),
              fluidRow(
                column(6,
                  numericInput("Stress_Management_Score","Stress Mgmt (1-10):", 6, 1, 10, step=0.5),
                  numericInput("Hydration_Liters","Hydration (L/day):", 2.0, 0.5, 5, step=0.1),
                  numericInput("Fiber_Intake_grams","Fibre (g/day):", 25, 5, 50)
                ),
                column(6,
                  numericInput("Omega3_Intake_mg","Omega-3 (mg/day):", 1000, 0, 5000, step=100),
                  numericInput("Low_Impact_Exercise_Days","Exercise days/wk:", 3, 0, 7),
                  numericInput("BP_Monitoring_Days","BP monitor days/wk:", 2, 0, 7)
                )
              ),
              fluidRow(
                column(6,
                  radioButtons("Weight_Management_Success","Weight trend:", c("Maintained"=1,"Gained"=2,"Lost"=3), selected=1, inline=TRUE),
                  numericInput("Targeted_Nutritional_Score","Nutrition Score:", 6, 1, 10)
                ),
                column(6,
                  numericInput("Supervised_Activity_Minutes","Supervised Activity (min/wk):", 30, 0, 300),
                  numericInput("Symptom_Log_Days","Symptom log days/wk:", 1, 0, 7),
                  radioButtons("Emergency_Preparedness","Emergency prepared?", c("Yes"=2,"No"=1), selected=2, inline=TRUE)
                )
              ),
              actionButton("go","🩺 Predict Heart Disease Status", class="btn-danger btn-block"),
              br(),
              uiOutput("pred_output"),
              uiOutput("feedback_output")
            )
          ),
          # Right: chatbot
          column(7,
            box(width=12, title="💬 Heart Health Chatbot", status="info", solidHeader=TRUE,
              div(class="chat-container", id="chatbox", uiOutput("chatMessages")),
              div(class="chat-input-row",
                textInput("userMsg", label=NULL, placeholder="Ask about inputs, results, or heart health…"),
                actionButton("sendMsg","Send", class="btn-info")
              )
            )
          )
        )
      )
    )
  )
)


# ===================== SERVER =====================

server <- function(input, output, session) {

  # Chat state
  chat_history <- reactiveVal(list(
    list(role="bot",
         text="👋 Hello! I'm your **Heart Health Assistant**.\n\nFill in the patient details and click **Predict** to get a heart disease risk estimate. Then ask me to explain any input, or to interpret your result!\n\nType *\"help\"* to see all topics I can help with.")
  ))
  pred_result <- reactiveVal(NULL)

  # ---- Prediction ----
  ee <- eventReactive(input$go, {
    obs <- data.frame(
      age=as.integer(input$age), sex=factor(input$sex, levels=levels(d$sex)),
      cp=factor(input$cp, levels=levels(d$cp)),
      trestbps=as.integer(input$trestbps), chol=as.integer(input$chol),
      fbs=factor(input$fbs, levels=levels(d$fbs)),
      restecg=factor(input$restecg, levels=levels(d$restecg)),
      thalach=as.integer(input$thalach),
      exang=factor(input$exang, levels=levels(d$exang)),
      oldpeak=as.numeric(input$oldpeak),
      slope=factor(input$slope, levels=levels(d$slope)),
      ca=factor(input$ca, levels=levels(d$ca)),
      thal=factor(input$thal, levels=levels(d$thal)),
      Risk_Level=factor(input$Risk_Level, levels=levels(d$Risk_Level)),
      Healthy_Diet_Score=as.numeric(input$Healthy_Diet_Score),
      Regular_Physical_Activity=factor(input$Regular_Physical_Activity, levels=levels(d$Regular_Physical_Activity)),
      Stress_Management_Score=as.numeric(input$Stress_Management_Score),
      Sleep_Hours=as.numeric(input$Sleep_Hours),
      Hydration_Liters=as.numeric(input$Hydration_Liters),
      Low_Impact_Exercise_Days=as.integer(input$Low_Impact_Exercise_Days),
      Fiber_Intake_grams=as.numeric(input$Fiber_Intake_grams),
      Omega3_Intake_mg=as.numeric(input$Omega3_Intake_mg),
      Weight_Management_Success=factor(input$Weight_Management_Success, levels=levels(d$Weight_Management_Success)),
      BP_Monitoring_Days=as.integer(input$BP_Monitoring_Days),
      Alcohol_Units_per_Week=as.numeric(input$Alcohol_Units_per_Week),
      Targeted_Nutritional_Score=as.numeric(input$Targeted_Nutritional_Score),
      Supervised_Activity_Minutes=as.numeric(input$Supervised_Activity_Minutes),
      Smoking_Status=factor(input$Smoking_Status, levels=levels(d$Smoking_Status)),
      Symptom_Log_Days=as.integer(input$Symptom_Log_Days),
      Emergency_Preparedness=factor(input$Emergency_Preparedness, levels=levels(d$Emergency_Preparedness))
    )
    round(predict(myModel1, newdata=obs, type="prob")[,1], digits=2)
  })

  observeEvent(input$go, {
    prob <- ee()
    pred_result(prob)
    # Auto-add prediction to chat
    pct <- round(prob * 100, 1)
    level <- if (prob > 0.7) "⚠️ High risk" else if (prob > 0.4) "🔶 Moderate risk" else "✅ Lower risk"
    bot_msg <- paste0("🩺 **Prediction complete!**\n\nProbability of heart disease: **", pct,
                      "%** — ", level, "\n\nAsk me *\"what does my result mean?\"* for detailed guidance, or ask about any specific input!")
    h <- chat_history()
    h <- c(h, list(list(role="bot", text=bot_msg)))
    chat_history(h)
  })

  output$pred_output <- renderUI({
    req(pred_result())
    p <- pred_result()
    color <- if (p > 0.5) "#c0392b" else "#27ae60"
    div(style=paste0("color:", color, "; font-size:1.1em; font-weight:bold; margin-top:10px;"),
        paste0("Heart Disease Probability: ", round(p*100,1), "%"))
  })

  output$feedback_output <- renderUI({
    req(pred_result())
    p <- pred_result()
    if (p > 0.5)
      p("We recommend consulting a doctor promptly. Meanwhile, reduce fatty foods, avoid excess salt, and exercise daily.")
    else
      p("✅ Lower heart disease risk detected. Keep up healthy eating habits and light daily exercise!")
  })

  # ---- Chat ----
  observeEvent(input$sendMsg, {
    req(input$userMsg != "")
    user_text <- input$userMsg
    updateTextInput(session, "userMsg", value="")
    bot_text <- chatbot_response(
      msg        = user_text,
      pred_prob  = pred_result(),
      pred_done  = !is.null(pred_result())
    )
    h <- chat_history()
    h <- c(h,
           list(list(role="user", text=user_text)),
           list(list(role="bot",  text=bot_text)))
    chat_history(h)
  })

  output$chatMessages <- renderUI({
    msgs <- chat_history()
    tags$div(
      lapply(msgs, function(m) {
        cls <- if (m$role=="user") "msg-user" else "msg-bot"
        div(class=cls, HTML(commonmark::markdown_html(m$text)))
      })
    )
  })

  # ---- EDA Plots ----
  output$plot_age <- renderPlot({
    sub_df <- subset(df, age < input$filter_age)
    ggplot(sub_df, aes(x=age, fill=heart_disease)) +
      geom_density(alpha=0.6) + labs(x="Age", y="Density") + theme_minimal()
  })
  output$plot_gender <- renderPlot({
    sub_df <- subset(df, Gender %in% input$filter_gender)
    ggplot(sub_df, aes(x=Gender, fill=heart_disease)) +
      geom_bar() + labs(x="Gender", y="Count") + theme_minimal()
  })
  output$plot_bp <- renderPlot({
    sub_df <- subset(df, trestbps < input$filter_bp)
    ggplot(sub_df, aes(x=trestbps, fill=heart_disease)) +
      geom_histogram(bins=25) + labs(x="Resting BP", y="Count") + theme_minimal()
  })
  output$plot_cp <- renderPlot({
    sub_df <- subset(df, Chest_Pain_type %in% input$filter_cp)
    ggplot(sub_df, aes(x=Chest_Pain_type, fill=heart_disease)) +
      geom_bar() + labs(x="Chest Pain Type", y="Count") + theme_minimal() +
      theme(axis.text.x=element_text(angle=20, hjust=1))
  })
  output$plot_chol <- renderPlot({
    sub_df <- subset(df, chol < input$filter_chol)
    ggplot(sub_df, aes(x=chol, fill=heart_disease)) +
      geom_density(alpha=0.6) + labs(x="Cholesterol", y="Density") + theme_minimal()
  })
  output$plot_thalach <- renderPlot({
    sub_df <- subset(df, thalach < input$filter_thalach)
    ggplot(sub_df, aes(x=thalach, fill=heart_disease)) +
      geom_histogram(bins=25) + labs(x="Max Heart Rate", y="Count") + theme_minimal()
  })
  output$plot_exang <- renderPlot({
    sub_df <- subset(df, exercise_angina %in% input$filter_exang)
    ggplot(sub_df, aes(x=exercise_angina, fill=heart_disease)) +
      geom_bar() + labs(x="Exercise Angina", y="Count") + theme_minimal()
  })
  output$plot_thal <- renderPlot({
    sub_df <- subset(df, thal_report %in% input$filter_thal)
    ggplot(sub_df, aes(x=thal_report, fill=heart_disease)) +
      geom_bar() + labs(x="Thal Report", y="Count") + theme_minimal() +
      theme(axis.text.x=element_text(angle=15, hjust=1))
  })
  output$plot_risk <- renderPlot({
    sub_df <- subset(df, Risk_Level_report %in% input$filter_risk)
    ggplot(sub_df, aes(x=Risk_Level_report, fill=heart_disease)) +
      geom_bar() + labs(x="Risk Level", y="Count") + theme_minimal()
  })
  output$plot_smoke <- renderPlot({
    sub_df <- subset(df, Smoking_Status_report %in% input$filter_smoke)
    ggplot(sub_df, aes(x=Smoking_Status_report, fill=heart_disease)) +
      geom_bar() + labs(x="Smoking Status", y="Count") + theme_minimal()
  })
  output$plot_diet <- renderPlot({
    ggplot(df, aes(x=Healthy_Diet_Score, y=Hydration_Liters, color=heart_disease)) +
      geom_point(alpha=0.5) + geom_smooth(method="loess", se=FALSE) +
      labs(x="Diet Score", y="Hydration (L)") + theme_minimal()
  })
  output$plot_sleep <- renderPlot({
    ggplot(df, aes(x=Sleep_Hours, y=Stress_Management_Score, color=heart_disease)) +
      geom_point(alpha=0.5) + geom_smooth(method="loess", se=FALSE) +
      labs(x="Sleep Hours", y="Stress Mgmt Score") + theme_minimal()
  })
}

shinyApp(ui=ui, server=server)
