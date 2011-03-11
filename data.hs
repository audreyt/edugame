
actions =
    [ Action 1 "面談" 0 "✆ 一名學生。"
        "彼此瞭解，是第一步。"
    , Action 2 "面談" 0 "✆ 一名學生。"
        "每個人的生命，都是說不完的故事。"
    , Action 3 "回應需求" 0 "本回合所出的教學卡，\n不需擲親和骰。"
        "說不出口的才算數。"
    , Action 4 "弱勢生名額" 0 "放入一張新的學生卡，面向下。"
        "給學習一個機會。"
    , Action 5 "試教" 0 "取回一張由自已所出，\n最頂端的教學卡。"
        "要瞭解現場教學，只有去現場教學。"
    , Action 6 "試讀" 1 "將所有學生卡翻成正面。"
        "要熟悉教學現場，只有到教學現場。"
    , Action 7 "記取經驗" 1 "取回一張棄牌堆中的教育卡。"
        "若沒有記錄，過去的就過去了。"
    , Action 8 "記取經驗" 1 "取回一張棄牌堆中的教育卡。"
        "創造始於摹仿。"
    , Action 9 "記取經驗" 1 "取回一張棄牌堆中的教育卡。"
        "從跌倒的地方站起來。"
    , Action 10 "轉學" 2 "立即結算一張學生卡，\n再放入一名新的學生，面向下。"
        "沒有一所學校適合所有人。"
    , Action 11 "典範效應" 2 "將一張場上的技藝卡改為環境卡。"
        "臨而後可觀。"
   ]

students =
    [ Student 1 "沈括" [A,K,R,V] (1) (12) [a,c,m,n,p,s] []
        "博學善文，於天文、方志、律曆、音樂、\n醫藥、卜算無所不通，皆有所論著。"
    , Student 2 "梵谷" [V] (2) (8) [a,s] [c,e,n,p,s]
        "愈加思索，我愈覺得沒有什麼藝術，\n比對人群的愛更純粹。"
    , Student 3 "莫札特" [A] (2) (8) [a,e] [c,m,n,s]
        "旁人的讚美和謾罵我全不放在心上。\n我只跟著自己的感覺走。"
    , Student 4 "徐光啟" [R] (2) (8) [e,m] [a,e,p]
        "《幾何原本》者，度數之宗，\n所以窮方圓平直之情，\n盡規矩準繩之用也。"
    , Student 5 "鐵木真" [K] (2) (8) [p,s] [c,e,m,p]
        "我能征服世界，但不能征服死亡。"
    , Student 6 "巴哈" [A,R] (1) (5) [a,m] [e,m,p]
        "我非勤勉不可，誰能像我一樣的勤勉，\n就能有同樣的成就。"
    , Student 7 "李清照" [A,R] (3) (7) [a,c] [a,m,n,p]
        "此情無計可消除，才下眉頭，卻上心頭。"
    , Student 8 "法布爾" [K,V] (1) (5) [n,s] [a,m,n,s]
        "要遠離主流的道路，才能有新的發現。"
    , Student 9 "阿基米德" [K,V] (3) (7) [m,n] [a,e,n,s]
        "不要碰我的圖！"
    , Student 10 "李白" [R,V] (1) (5) [a,c] [a,n,p,s]
        "鐘鼓饌玉何足貴，但願長醉不願醒。"
    , Student 11 "達爾文" [R,V] (3) (7) [e,n] [e,m,p]
        "和友情相比，名聲、榮譽、快樂和財富，\n都是塵土。"
    , Student 12 "張衡" [A,K] (1) (5) [m,n] [a,m,p,s]
        "願竭力以守義兮，雖貧窮而不改。"
    , Student 13 "鄧肯" [A,K] (3) (7) [a,p] [a,m,s]
        "自由的身體蘊藏最高的智慧。"
    , Student 14 "卡森" [A,V] (1) (5) [e,n] [m,n,s]
        "我們知道，大地不屬於人類，\n而人類屬於大地。"
    , Student 15 "屈原" [A,V] (3) (7) [c,s] [m,n,p,s]
        "國無人莫我知兮，又何懷乎故都？\n既莫足爲美政兮，吾將從彭咸之所居。"
    , Student 16 "貞德" [K,R] (1) (5) [p,s] []
        "比起刀和劍，我更重視我的那一面旗。"
    , Student 17 "居禮夫人" [K,R] (3) (7) [n] [p,s]
        "生命中的一切不足以恐懼，只足以瞭解。"
    , Student 18 "愛迪生" [K,V,Anti R] (3) (9) [a,n,s] [a,p,s]
        "我的發明都不是湊巧。\n我一次又一次實驗，直到它化為現實。"
    , Student 19 "薩提爾" [A,K,R] (3) (9) [e,n,s] [a,c,n]
        "我要成為幫小孩觀察大人的偵探。"
    , Student 20 "愛因斯坦" [A,V,Anti R] (3) (9) [m,n,s] [m,p,s]
        "我並不是很聰明，\n只是和問題相處得久一點。"
    , Student 21 "法拉第" [A,K,V] (3) (9) [a,n,p] [m,n,s]
        "插上了想像的翅膀，科學就能得勝。"
    , Student 22 "克里希那穆提" [] (1) (4) [s] []
        "年幼，無心向學，體弱而好幻。\n其師鄙之，常施毒打。父亦如是。\n然克氏處之泰然，寵辱不驚。"
    , Student 23 "海倫凱勒" [K,R,Anti A,Anti V] (3) (10) [e,s] [e,s]
        "離開了井後，我渴望學習：\n因為每樣東西都有一個名字。"
    , Student 24 "顏回" [A,R,V,Anti K] (3) (10) [c,m,p,s] [p,s]
        "願無伐善，無施勞。"
    , Student 25 "項羽" [A,K,V,Anti R] (3) (10) [c,p,s] [c,m,s]
        "書足以記名姓而已。劍一人敵，不足學，\n學萬人敵。"
    , Student 26 "鮑比" [A,Anti K,Anti R,Anti V] (3) (10) [a,e] [p,s]
        "在宇宙中是否有一把鑰匙，\n可以解開我的潛水鐘？"
   ]

lessons =
    [ Lesson 1 "放牛吃草" [K] (1) (-999) [a,p] []
        "就自由活動吧。"
    , Lesson 2 "放牛吃草" [A] (1) (-999) [c,e,s] []
        "先去辦點事。"
    , Lesson 3 "放牛吃草" [R] (1) (-999) [m,n] []
        "安靜自習！"
    , Lesson 4 "照本宣科" [R] (0) (0) [m,n] []
        "課本編得不好怎麼辦？"
    , Lesson 5 "照本宣科" [A] (0) (0) [m,n] []
        "有點不負責任。"
    , Lesson 6 "照本宣科" [R] (0) (0) [a,p] []
        "知識和迷信的界限在哪裡？"
    , Lesson 7 "照本宣科" [A] (0) (0) [a,p] []
        "站在巨人的腳印上。"
    , Lesson 8 "照本宣科" [R] (0) (0) [c,e,s] []
        "教育常變成複製。"
    , Lesson 9 "照本宣科" [A] (0) (0) [c,e,s] []
        "編課本使人成長，抄課本很難成長。"
    , Lesson 10 "發現教學" [R,V] (3) (0) [m,n] []
        "人不能在真空中學習。"
    , Lesson 11 "發現教學" [A,V] (3) (0) [c,e,s] []
        "小孩是天生的科學家。"
    , Lesson 12 "發現教學" [K,V] (3) (0) [a,p] []
        "結論讓他自己下。"
    , Lesson 13 "討論教學" [A,V] (3) (0) [m,n] []
        "能給問題，比給答案更重要。"
    , Lesson 14 "討論教學" [A,R] (3) (0) [c,e,s] []
        "要交換意見，更要交換意見背後的視野。"
    , Lesson 15 "討論教學" [A,K] (3) (0) [a,p] []
        "引發創意，不是灌輸知識。"
    , Lesson 16 "實驗仿作" [K,V] (3) (0) [a,n] []
        "實驗在理論之前。"
    , Lesson 17 "實驗仿作" [K,R] (3) (0) [m,n] []
        "思考實驗，也是一種實驗。"
    , Lesson 18 "實驗仿作" [A,K] (3) (0) [n,s] []
        "雖然早已預知結果，實驗還是令人驚奇。"
    , Lesson 19 "趣味數學" [A,K,R] (0) (0) [a,m] [i]
        "由趣味的數學，進入數學的趣味。"
    , Lesson 20 "趣味科學" [A,K,R] (0) (0) [n,s] [i]
        "偶而裝傻好像也不錯。"
    , Lesson 21 "武術入門" [K,V] (1) (1) [a,p] []
        "強身，健體，壯膽。"
    , Lesson 22 "舞蹈入門" [A,K] (1) (1) [a,p] []
        "從律動開始，讓身體自由。"
    , Lesson 23 "歌唱入門" [A] (1) (1) [a,c,e] []
        "欣賞自己的聲音。"
    , Lesson 24 "精研武術" [K,V] (4) (-999) [a,p] []
        "不爭不鬥，止戈為武。"
    , Lesson 25 "精研舞蹈" [A,K] (4) (-999) [a,p] []
        "台上一分鐘，台下十年功。"
    , Lesson 26 "精研歌唱" [A] (4) (-999) [a,c,e] []
        "歌唱的技巧，聲音的表情。"
    , Lesson 27 "指定閱讀" [R] (4) (-999) [c,e,s] []
        "問題是他肯不肯翻開第一頁？"
    , Lesson 28 "指定閱讀" [R] (4) (-999) [a,m,n,p] []
        "有時教師比學生更了解他的需求。"
    , Lesson 29 "自由閱讀" [R] (2) (2) [c,n,p,s] []
        "像風一樣，你不停地跑。"
    , Lesson 30 "自由閱讀" [R] (2) (2) [a,e,m] []
        "被課本所限，就註定無知了。"
    , Lesson 31 "基本能力" [A,K,V] (0) (4) [m,n] [u]
        "跳出死胡同，認識真學問。"
    , Lesson 32 "基本能力" [A,K,R] (0) (4) [c,e,s] [u]
        "不要在沙上蓋塔。"
    , Lesson 33 "自學指導" [A,R,V] (4) (0) [m,n] [l]
        "要怎麼檢查自己會不會？"
    , Lesson 34 "自學指導" [A,K,R] (4) (0) [c,e,s] [l]
        "知識是用拿的，不是用給的。"
    , Lesson 35 "獨立研究" [] (7) (-999) [] [l]
        "最好的老師，讓學生離開他之後，\n也學得好。"
    , Lesson 36 "個別輔導" [A] (1) (3) [] [l]
        "許多人的教育之路，由家教和課輔開始。"
    , Lesson 37 "個別輔導" [A] (1) (3) [] [l]
        "最有效的診斷，是一對一。"
    , Lesson 38 "小階梯教學" [A,V] (-999) (3) [] [u]
        "知識若切太細，雖然可以跟隨，\n卻會失去意義。"
    , Lesson 39 "小階梯教學" [K,V] (-999) (3) [] [u]
        "有人需要一步一步慢慢來。"
    , Lesson 40 "小階梯教學" [R,V] (-999) (3) [] [u]
        "千里之行，始於足下。"
    , Lesson 41 "圖表分析" [R,V] (2) (2) [n,s] []
        "圖表自己會說話。"
    , Lesson 42 "資料分析" [R] (2) (2) [m,s] []
        "表示法決定了資料的意義。"
    , Lesson 43 "圖象秩序" [V] (2) (2) [a,m] []
        "一目了然。"
    , Lesson 44 "山訓" [K,V] (2) (2) [n,p] [u]
        "我們爬的是不同的山。"
    , Lesson 45 "情境教學" [A] (2) (2) [a,e] [u]
        "身歷其境。"
    , Lesson 46 "詩作賞析" [R] (2) (2) [c,e] [u]
        "詩是文字的結晶。"
    , Lesson 47 "戲劇社" [A,K] (2) (2) [a,p,s] [u]
        "戴過許多面具，再來尋找自己。"
    , Lesson 48 "籃球社" [K,V] (2) (2) [p,s] [u]
        "打球打球！"
    , Lesson 49 "寫作工坊" [A,R] (2) (2) [a,c] [u]
        "創作是為了交流。"
    , Lesson 50 "科學史" [R] (4) (4) [m,n,s] []
        "掌握原始問題，就掌握了知識的大半。"
    , Lesson 51 "語文史" [R] (4) (4) [c,e,s] []
        "博古而能通今。"
    , Lesson 52 "藝術史" [R] (4) (4) [a,s] []
        "簡潔與合一，是美的泉源。"
    , Lesson 53 "華德福教學" [K,V] (3) (3) [a,n,p] []
        "透過身體，將意志帶進思考。"
    , Lesson 54 "蒙特梭利教學" [K,R] (3) (3) [a,m,s] []
        "不必主動去教，只需觀察、引導、解惑。"
    , Lesson 55 "蘇格拉底教學" [A,K] (3) (3) [e,m,n] []
        "我知我無知，所以就去問。"
    , Lesson 56 "主題探索" [K,R,V] (4) (2) [m,n,s] [u]
        "知識無法傳授，只能重新發生。"
    , Lesson 57 "合作討論" [A,K,R] (4) (2) [c,m,s] [u]
        "良好的討論，不是爭辯，而是合作。"
    , Lesson 58 "任務導向" [A,K,V] (4) (2) [n,p,s] [u]
        "先彷作，再創作；有目標，則集中。"
    , Lesson 59 "經典研讀" [R] (5) (5) [c] []
        "看解說不如讀原典。"
    , Lesson 60 "會話教學" [A] (5) (5) [e] []
        "Here we speak \nEnglish only."
    , Lesson 61 "體能集訓" [K] (5) (5) [p] []
        "苦練、苦練、再苦練。"
    , Lesson 62 "啟蒙導師遊戲" [A,K,R,V] (4) (4) [s] [i]
        "請支持自創遊戲！"
    , Lesson 63 "魔術入門" [A,K] (1) (1) [a,s] []
        "一、二、三，變！"
    , Lesson 64 "精研魔術" [A,K] (4) (-999) [a,s] []
        "要讓人看到的，就要表現清楚。\n其他的一切，全都隱形。"
    , Lesson 65 "程式入門" [K,R] (1) (1) [a,e] []
        "練習把話說清楚。"
    , Lesson 66 "精研程式" [K,R] (4) (-999) [a,e] []
        "碼就是資料，資料就是碼。"
    , Lesson 67 "繪畫入門" [K,V] (1) (1) [a,n] []
        "從敢畫開始，不考慮對錯。"
    , Lesson 68 "精研繪畫" [K,V] (4) (-999) [a,n] []
        "不只描寫現實，而是創造現實。"
   ]

skills =
    [ Skill 1 "以火點火" "教學額外具有 ♥ 能力。"
        "授己所愛，愛己所授。"
    , Skill 2 "以心傳心" "可無視學習風格的影響。"
        "佛陀拈花，迦葉微笑。"
    , Skill 3 "同理" "教學額外具有 ✙ 能力。"
        "向童年的自己學習。"
    , Skill 4 "巧思" "教學皆獲得 +1 力道。"
        "尺有所短，寸有所長。"
    , Skill 5 "圖象" "教學可額外具有或略去視覺風格。"
        "圖象比文字更鮮明。"
    , Skill 6 "語言" "教學可額外具有或略去聽覺風格。"
        "語言是一種藝術。"
    , Skill 7 "文字" "教學可額外具有或略去閱讀風格。"
        "凡能寫的，都能寫清楚。"
    , Skill 8 "實作" "教學可額外具有或略去操作風格。"
        "學以致用，從做中學。"
    , Skill 9 "幽默" "無興趣時，蒙昧值取左。"
        "他們喜歡的，是學問本身？"
    , Skill 10 "敏銳" "只需 1 ⏎ 即可觀察學生。"
        "讓小孩教你怎樣教他。"
    , Skill 11 "敏銳" "只需 1 ⏎ 即可觀察學生。"
        "先放下著急與成見。"
    , Skill 12 "親和" "親和骰擲出 2 亦可成功。"
        "大表總比老師好。"
    , Skill 13 "親和" "親和骰擲出 2 亦可成功。"
        "放下身段，以身作則。"
    , Skill 14 "專業" "額外壓印一張教學卡：\n對此卡每一學門，教學力道 +3。"
        "教學技巧好練，學門專業難修。"
    , Skill 15 "專業" "額外壓印一張教學卡：\n對此卡每一學門，教學力道 +3。"
        "聞道有先後，術業有專攻。"
   ]

environments =
    [ Environment 1 "獨立教育工作者社群" "技藝卡皆改為環境卡，且只需 0 \n⏎。"
        "交流採借，就會成長。"
    , Environment 2 "師生人格平等" "所有人的親和骰點數 +1。"
        "沒有絕對的施與受。"
   ]

assistants =
    [ Assistant 1 "萬事通" [R,Anti A] [a,p] [u] 2
        "讀萬卷書，行萬里路。"
    , Assistant 2 "大聲公" [A,Anti R] [s] [u] 2
        "沒關係，再說一遍。"
    , Assistant 3 "小畫家" [V,Anti K] [c,e] [u] 2
        "這樣看，明白了嗎？"
    , Assistant 4 "金手指" [K,Anti V] [m,n] [u] 2
        "常有欲，以觀其徼。"
    , Assistant 5 "妙博士" [] [] [i] 4
        "常無欲，以觀其妙。"
   ]
