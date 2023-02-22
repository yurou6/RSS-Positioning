# RSS-Positioning
## 無線網路定位方法實作
使用 RSS 訊號地圖的定位方法。


需求
- 實作 1NN、3NN、5NN 比對方法，計算定位錯誤率。
- 在 google map 上找出預測的座標點。


實作
1. 使用UJIIndoorLoc dataset，先讀入trainingData.csv和validationData.csv，分別作為訓練集和測試集資料，檢查資料中是否有NA，若有NA用100取代。
2. 每個位置有520個AP，過濾訓練集和測試集資料，將資料中的訊號資料和經緯度資料分開儲存。
3. 定義矩陣，分別儲存1NN、3NN、5NN誤差值資料。
4. 原始資料裡，100表示沒有收到訊號，找訓練集裡訊號資料中最小的數字，將訓練集和測試集的訊號資料轉為正數
5. 找訓練集和測試集的row長度。
6. 利用兩個for迴圈，將validationData中的fingerprints去跟radio database中每一項計算距離，再進行比較，找出最近距離。
7. 將所有算出的距離放進一vector中。
8. 使用order找出vector中前五項最近距離的index。
9. 使用到上述的index，分別進行1NN、3NN、5NN的距離誤差計算，其中3NN和5NN要先將座標值平均，再與實際位置計算距離誤差。
10. 以hist這個function來繪製直方圖，分別有1NN、3NN、5NN直方圖。
11. 用write.table把誤差值放入csv檔中。
12. 將測試集資料換成plot.csv，檢查資料中是否有NA，若有NA用100取代。
13. 同樣先過濾資料，將資料中的訊號資料和經緯度資料分開儲存。
14. 定義矩陣，儲存誤差值資料。
15. 同樣將訓練集和測試集的訊號資料轉為正數。
16. 預設訓練集的第一個位置為最接近的位置，和其他訓練集位置相比後，若對應欄位相減的平方，小於最小誤差值的話，就將此位置視為最近位置，並記錄經緯度資料，最後和訓練集全部位置相比後，就能得到預估位置。