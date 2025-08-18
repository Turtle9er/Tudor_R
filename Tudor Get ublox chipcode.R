# Load the required libraries
library(httr)
library(jsonlite)

# 1. Define the API endpoint URL
# Replace this with the actual URL from your device profile
url <- "https://api.thingstream.io/ztp/assistnow/credentials"

# 2. Create the data payload as an R list
# Make sure to replace the placeholder values with your actual data
payload <- list(
  token = "4560d7bb-2edb-4bf7-8c21-7bfc791f2bb1", # Use your lowercase token
  messages = list(
    "UBX-SEC-UNIQID" = "B56227030A00020000009B51E98F2B541907", # Your unique ID
    "UBX-MON-VER" = "B5620A04BE00524F4D2053504720352E31302028376232303265290000000000000000003030304130303030000046575645523D53504720352E31300000000000000000000000000000000050524F545645523D33342E313000000000000000000000000000000000004D4F443D53414D2D4D3130510000000000000000000000000000000000004750533B474C4F3B47414C3B424453000000000000000000000000000000534241533B515A53530000000000000000000000000000000000000000003F84" # Your HW/FW version
  )
)

# 3. Send the POST request
# The 'httr' package automatically converts the list to a JSON string
# when you set encode = "json"
response <- POST(
  url = url,
  body = payload,
  encode = "json",
  add_headers("Content-Type" = "application/json")
)

# 4. Check the response
if (http_status(response)$category == "Success") {
  # Request was successful, print the content
  print("Request successful!")
  # The content is parsed from JSON automatically
  content_parsed <- content(response, "parsed")
  print(content_parsed)
  
  # You can access the chipcode like this (assuming the key is "chipcode")
  # chipcode <- content_parsed$chipcode
  # print(paste("Chipcode:", chipcode))
  
} else {
  # Request failed, print the status and error message
  print(paste("Request failed with status:", status_code(response)))
  print(content(response, "text"))
}
