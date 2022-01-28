cd front
start cmd.exe /wait /k "call npm install" pause 
start cmd.exe /k "call npm start" pause 
cd ../back
start cmd.exe /k "call gradlew bootRun" 

