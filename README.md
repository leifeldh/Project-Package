---
title: "Quality Inspection Tools"
author: "Heather Leifeld"
date: "Due 12/13/2021"
output: html_document
---




Introduction: This package was created to address a need for a statistical sampling tool within manufacturing environments. The medical device industry typically conforms to ANSI/ASQ Z1.4-2003 (R2013) standards when determining sample size for product quality inspections. A common tool used by inspectors is the AQL Inspector's Rule, however, automation through R could greatly improve task efficiency for quality inspectors.

Methods: The proposed method assumes an AQL level of 2.5, which is the standard quality level for major defects under "normal" inspection. The operator will approach this method with a product of a particular lot size that needs to be inspected. Product specifications will indicate a particular Inspection Level (1, 2, or 3). This lets the operator decide which function to use (InspectionLevel1, 2 or 3, respectively). Using the Inspection level family of functions will provide a sampling letter code, which is intermediate sampling information that can be used with the sampsize and rejectnum functions. The sampsize function takes an input of the sampling letter code and in return tells the operator how many samples must be pulled for inspection to assure acceptable quality levels. Likewise, the rejectnum fuction tells the operator how many samples can fail inspection before the entire lot must be rejected due to poor quality.

InspectionLevel1 - User inputs lot size and receives the sampling letter code.

InspectionLevel2 - User inputs lot size and receives the sampling letter code.

InspectionLevel3 - User inputs lot size and receives the sampling letter code.

sampsize- User inputs sampling letter code and receives sample size.

rejectnum- User inputs sampling letter code and receives lot rejection number.



```{r}

library(readxl)
data <- read_xlsx("C:/Users/heath/OneDrive/Desktop/dataset.xlsx")


#Inspection Level 1

InspectionLevel1 <-function(x){
  if(data[1,4] < x &  x < data[1,5])
     return(data[1,6])
  if(data[2,4] < x &  x < data[2,5])
     return(data[2,6])
  if(data[3,4] < x &  x < data[3,5])
     return(data[3,6])
  if(data[4,4] < x &  x < data[4,5])
     return(data[4,6])
  if(data[5,4] < x &  x < data[5,5])
     return(data[5,6])
  if(data[6,4] < x &  x < data[6,5])
     return(data[6,6])
  if(data[7,4] < x &  x < data[7,5])
     return(data[7,6])
  if(data[8,4] < x &  x < data[8,5])
     return(data[8,6])
  if(data[9,4] < x &  x < data[9,5])
     return(data[9,6])
  if(data[10,4] < x &  x < data[10,5])
     return(data[10,6])
  if(data[11,4] < x &  x < data[11,5])
     return(data[11,6])
  if(data[12,4] < x &  x < data[12,5])
     return(data[12,6])
  if(data[13,4] < x &  x < data[13,5])
     return(data[13,6])
  if(data[14,4] < x &  x < data[14,5])
     return(data[14,6])
  if(data[15,4] < x &  x < data[15,5])
     return(data[15,6])
}

#Inspection Level 2

InspectionLevel2<-function(x){
  if(data[1,4] < x &  x < data[1,5])
     return(data[1,7])
  if(data[2,4] < x &  x < data[2,5])
     return(data[2,7])
  if(data[3,4] < x &  x < data[3,5])
     return(data[3,7])
  if(data[4,4] < x &  x < data[4,5])
     return(data[4,7])
  if(data[5,4] < x &  x < data[5,5])
     return(data[5,7])
  if(data[6,4] < x &  x < data[6,5])
     return(data[6,7])
  if(data[7,4] < x &  x < data[7,5])
     return(data[7,7])
  if(data[8,4] < x &  x < data[8,5])
     return(data[8,7])
  if(data[9,4] < x &  x < data[9,5])
     return(data[9,7])
  if(data[10,4] < x &  x < data[10,5])
     return(data[10,7])
  if(data[11,4] < x &  x < data[11,5])
     return(data[11,7])
  if(data[12,4] < x &  x < data[12,5])
     return(data[12,7])
  if(data[13,4] < x &  x < data[13,5])
     return(data[13,7])
  if(data[14,4] < x &  x < data[14,5])
     return(data[14,7])
  if(data[15,4] < x &  x < data[15,5])
     return(data[15,7])
}


#Inspection Level 3


InspectionLevel3<-function(x){
  if(data[1,4] < x &  x < data[1,5])
     return(data[1,8])
  if(data[2,4] < x &  x < data[2,5])
     return(data[2,8])
  if(data[3,4] < x &  x < data[3,5])
     return(data[3,8])
  if(data[4,4] < x &  x < data[4,5])
     return(data[4,8])
  if(data[5,4] < x &  x < data[5,5])
     return(data[5,8])
  if(data[6,4] < x &  x < data[6,5])
     return(data[6,8])
  if(data[7,4] < x &  x < data[7,5])
     return(data[7,8])
  if(data[8,4] < x &  x < data[8,5])
     return(data[8,8])
  if(data[9,4] < x &  x < data[9,5])
     return(data[9,8])
  if(data[10,4] < x &  x < data[10,5])
     return(data[10,8])
  if(data[11,4] < x &  x < data[11,5])
     return(data[11,8])
  if(data[12,4] < x &  x < data[12,5])
     return(data[12,8])
  if(data[13,4] < x &  x < data[13,5])
     return(data[13,8])
  if(data[14,4] < x &  x < data[14,5])
     return(data[14,8])
  if(data[15,4] < x &  x < data[15,5])
     return(data[15,8])
}

# Sample size generator

sampsize <- function(y){
  if(y == data[1,1]) return(data[1,2])
  if(y == data[2,1]) return(data[2,2])
  if(y == data[2,1]) return(data[3,2])
  if(y == data[4,1]) return(data[4,2])
  if(y == data[5,1]) return(data[5,2])
  if(y == data[6,1]) return(data[6,2])
  if(y == data[7,1]) return(data[7,2])
  if(y == data[8,1]) return(data[8,2])
  if(y == data[9,1]) return(data[9,2])
  if(y == data[10,1]) return(data[10,2])
  if(y == data[11,1]) return(data[11,2])
  if(y == data[12,1]) return(data[12,2])
  if(y == data[13,1]) return(data[13,2])
  if(y == data[14,1]) return(data[14,2])
  if(y == data[15,1]) return(data[15,2])
}


#Rejection Number

rejectnum <- function(z){
  if(z == data[1,1]) return(data[1,3])
  if(z == data[2,1]) return(data[2,3])
  if(z == data[2,1]) return(data[3,3])
  if(z == data[4,1]) return(data[4,3])
  if(z == data[5,1]) return(data[5,3])
  if(z == data[6,1]) return(data[6,3])
  if(z == data[7,1]) return(data[7,3])
  if(z == data[8,1]) return(data[8,3])
  if(z == data[9,1]) return(data[9,3])
  if(z == data[10,1]) return(data[10,3])
  if(z == data[11,1]) return(data[11,3])
  if(z == data[12,1]) return(data[12,3])
  if(z == data[13,1]) return(data[13,3])
  if(z == data[14,1]) return(data[14,3])
  if(z == data[15,1]) return(data[15,3])
}


```

Results

```{r}

#Applying the functions for a lot size of 1500, inspection level2:

InspectionLevel2(1500)

#Input the result "K" into sampsize function:

sampsize("K")

#Output tells the inspector to pull and inspect 125 samples. After inspecting, check the reject number:

rejectnum("K")

#Tells the inspector that 8 rejects will require a failure of the entire lot.

```


Conclusion


In conclusion, this package could hopefully be a tool to improve operational efficiencies. Since there are additional "special" inspection levels that apply to special circumstances, a future project could potentially apply the same concept to a wider range of inspections. Additionally, there are times when an inspector may want to perform a tighter inspection (such as the case of critical risk of harm to the patient). This concept could therefore be applied to tightened inspections at a tighter AQL level. As a final note, I struggled with applying my functions row-wise and across columns, resulting in some bulky code. I hope to be able to condense this to more concise functions in the future.
