
actions =
    [ Action "面談" 0 "✆ 一名學生。"
        "彼此瞭解，建立共同願景，是第一步。"
    , Action "面談" 0 "✆ 一名學生。"
        "好的開始，是成功的一半。"
    , Action "試讀" 0 "將所有學生卡翻成正面。"
        "不妨試試再說。"
    , Action "轉學" 2 "移除一張授課未完的學生卡，並放入一張新的，面向下。"
        "在家庭和學校的不同理念間拔河，不如轉到理念一致的學校。"
    , Action "弱勢生免費名額" 1 "放入一張新的學生卡，面向下。"
        "雖然財務拮据，自主培力學園還是為弱勢生保留免費名額。"
    , Action "記取經驗" 1 "取回一張棄牌堆中的教育卡。"
        "失敗為成功之母。"
    , Action "記取經驗" 1 "取回一張棄牌堆中的教育卡。"
        "不遷怒，不二過。"
    , Action "記取經驗" 1 "取回一張棄牌堆中的教育卡。"
        "擇其善者而從之，其不善者改之。"
   ]

students =
    [ Student "沈括" [A,K,R,V] 1 9 [a,c,m,n,p,s] []
        "博學善文，於天文、方志、律曆、音樂、醫藥、卜算無所不通，皆有所論著。"
    , Student "梵谷" [V] 2 8 [a,s] [c,e,n,p,s]
        "愈加思索，我愈覺得沒有什麼藝術，比對人群的愛更純粹。"
    , Student "莫札特" [A] 2 8 [a,p] [c,m,n,s]
        "旁人的讚美和謾罵我全不放在心上。我只跟著自己的感覺走。"
    , Student "徐光啟" [R] 2 8 [e,m] [a,e,p]
        "余以為小用、大用，實在其人。"
    , Student "鐵木真" [K] 2 8 [p,s] [c,e,m,p]
        "我能征服世界，但不能征服死亡。"
    , Student "巴哈" [A,R] 1 5 [a,m] [e,m,p]
        "我非勤勉不可，誰能像我一樣的勤勉，就能有同樣的成就。"
    , Student "李清照" [A,R] 3 6 [a,c] [a,m,n,p]
        "此情無計可消除，才下眉頭，卻上心頭。"
    , Student "法布爾" [K,V] 1 5 [n,s] [a,m,n,s]
        "你們把昆蟲肢解，而我是研究活生生的昆蟲。"
    , Student "阿基米德" [K,V] 3 6 [m,n] [a,e,n,s]
        "不要動我的圖！"
    , Student "李白" [R,V] 1 5 [a,c] [a,n,p,s]
        "君不見，黃河之水天上來，奔流到海不復回。"
    , Student "達爾文" [R,V] 3 6 [e,n] [e,m,p]
        "名聲、榮譽、快樂、財富，如果同友情相比，它們都是塵土。"
    , Student "張衡" [A,K] 1 5 [m,n] [a,m,p,s]
        "願竭力以守義兮，雖貧窮而不改。"
    , Student "鄧肯" [A,K] 3 6 [a,p] [a,m,s]
        "最自由的身體蘊藏最高的智慧。"
    , Student "艾雪" [A,V] 1 5 [a,m] [e,m,n,s]
        "我立體幾何稍好，因為和想像能共鳴。即使如此，在學校也沒唸得很好。"
    , Student "屈原" [A,V] 3 6 [c,s] [m,n,p,s]
        "國無人莫我知兮，又何懷乎故都？既莫足爲美政兮，吾將從彭咸之所居。"
    , Student "愛迪生" [K,R,V] 3 7 [a,n,s] [a,p,s]
        "我的發明都不是碰巧的。我一次又一次地實驗，直到它化為現實。"
    , Student "薩提爾" [A,K,R] 3 7 [e,n,s] [a,c,n]
        "我要成為幫小孩觀察大人的偵探。"
    , Student "愛因斯坦" [A,R,V] 3 7 [m,n,s] [m,p,s]
        "我並不是很聰明，只是和問題相處得久一點。"
    , Student "法拉第" [A,K,V] 3 7 [a,n,p] [m,n,s]
        "一旦科學插上幻想的翅膀，它就能贏得勝利。"
    , Student "克里希那穆提" [] 1 2 [s] []
        "年幼，無心向學，體弱而好幻。其師鄙之，常施毒打。父亦如是。然克氏處之泰然，寵辱不驚。"
   ]

lessons =
    [ Lesson "照本宣科" [A,R] 2 2 [c,s] []
        "雖然它被大量批評，但也有人能學到東西。"
    , Lesson "照本宣科" [A,R] 2 2 [n] []
        "站在巨人的腳印上。"
    , Lesson "照本宣科" [A,R] 2 2 [e,s] []
        "有些人認為，照本宣科，是一種不負責任的態度。"
    , Lesson "照本宣科" [A,R] 1 1 [m,n] []
        "當課本編的不好時，照本宣科非常危險。"
    , Lesson "照本宣科" [A,R] 2 2 [m] []
        "知識和迷信的界限在哪裡？"
    , Lesson "照本宣科" [A,R] 2 2 [c,e] []
        "教育常常變成一種複製。"
    , Lesson "照本宣科" [A,R] 1 1 [m,n] []
        "照本宣科雖然輕鬆，卻局限了教師的成長。"
    , Lesson "照本宣科" [A,R] 1 1 [c,e,s] []
        "編寫課本使人成長，照本宣科卻很難成長。"
    , Lesson "發現教學" [K,V] 3 1 [m,n] []
        "人不能在真空中學習。"
    , Lesson "發現教學" [K,V] 3 1 [m,n] []
        "老師能給問題，比能給答案更重要。"
    , Lesson "討論教學" [A] 3 1 [c,n,s] []
        "良好的討論，不是爭辯，而是合作。"
    , Lesson "討論教學" [A] 3 1 [c,n,s] []
        "不必交換意見，要交換意見背後的視野。"
    , Lesson "進階發現教學" [A,K,R,V] 4 1 [m,n,s] [u]
        "知識無法傳授，只能重新發生。"
    , Lesson "進階討論教學" [A,K,R,V] 4 2 [c,m,n,s] []
        "萬法歸宗，最後的道理總是相通。"
    , Lesson "獨立研究" [A,K,R,V] 5 0 [a,m,n] []
        "好的老師，讓學生在他的課上學得好。最好的老師，讓學生離開他之後，也學得好。"
    , Lesson "趣味數學" [A,K,R] 2 2 [m] [u]
        "由趣味的數學，進入數學的趣味。"
    , Lesson "趣味科學" [A,K,R] 2 2 [n] [u]
        "偶而裝傻好像也不錯。"
    , Lesson "蒙式教學" [K,R,V] 4 2 [m,n,s] []
        "讓小孩在豐富的情境中探索，以自身的好奇學習。老師不必主動去教，只需觀察、引導、解惑。"
    , Lesson "實驗彷作" [K] 3 3 [n,p] [u]
        "自己做過一遍，就不容易遺忘。"
    , Lesson "實驗彷作" [K,V] 3 3 [a,n] []
        "教學應摹擬知識的發生：實驗在理論之前。"
    , Lesson "實驗彷作" [K,R] 3 3 [m,n] []
        "思考實驗，也是一種實驗。"
    , Lesson "實驗彷作" [A,K] 3 3 [n,s] []
        "雖然不是自己設計，實驗本身還是令人驚奇。"
    , Lesson "華德福數學" [K,V] 3 3 [a,m] []
        "透過身體，將意志帶進思考。"
    , Lesson "華德福體育" [A,K] 3 3 [a,p] []
        "身心靈是一體的，體育不只是體育。"
    , Lesson "武術入門" [K,V] 2 2 [a,p] [u]
        "強身健體之外，還可以壯膽。"
    , Lesson "舞蹈入門" [A,K] 2 2 [a,p] [u]
        "有肢體的人，都會跳舞。"
    , Lesson "歌唱入門" [A] 2 2 [a,c,e] [u]
        "欣賞自己的聲音，人人都是音樂家。"
    , Lesson "精研武術" [K,V] 4 0 [a,p] []
        "善勝人者不爭，止戈為武。"
    , Lesson "精研舞蹈" [A,K] 4 0 [a,p] []
        "台上一分鐘，台下十年功。"
    , Lesson "精研歌唱" [A] 4 0 [a,c,e] []
        "旋律中的意志更有力。"
    , Lesson "指定閱讀" [R] 4 0 [c,e] []
        "問題是他肯不肯翻開第一頁？"
    , Lesson "指定閱讀" [R] 4 0 [m,n] []
        "有時教師比學生更了解他的需求。"
    , Lesson "自由閱讀" [R] 3 1 [c,e] []
        "如果真有興趣，自己就會去讀，何必當功課？"
    , Lesson "自由閱讀" [R] 3 1 [m,n] []
        "學海無涯。若被課本所限，你就註定無知了。"
    , Lesson "基本能力" [A,K,V] 1 4 [m,n] [u]
        "在自主培力學園，老師要在一年內扭轉學生不良的學習經驗，並培養出可以檢證的基本能力。"
    , Lesson "基本能力" [A,K,R] 1 4 [c,e,s] [u]
        "只要有一個想學的人沒有學到，一門課就不算是成功。"
    , Lesson "自學指導" [A,R,V] 4 1 [m,n] []
        "重點在如何檢查自己會不會。"
    , Lesson "自學指導" [A,K,R] 4 1 [c,e,s] []
        "知識是用拿的，不是用給的。"
    , Lesson "個別輔導" [A,V] 1 3 [] [l]
        "最有效的診斷，是一對一。"
    , Lesson "個別輔導" [A,K] 1 3 [] [l]
        "從玩伴，到典範，到老師，是自然的進路。倒過來可就難了。"
    , Lesson "個別輔導" [A,R] 1 3 [] [l]
        "許多人的教育之路，是從家教和課輔開始。"
    , Lesson "個別輔導" [A] 1 3 [] [l,u]
        "正如教練多半打不過選手，老師也不需要懂得比學生多。開拓他的視野才是重點。"
    , Lesson "小階梯教學" [A,V] 0 3 [] [u]
        "知識若切得太細，雖然可以跟隨，卻會失去意義。"
    , Lesson "小階梯教學" [K,V] 0 3 [] [u]
        "有些人需要一步一步慢慢來。"
    , Lesson "圖表分析" [V] 3 3 [n,s] []
        "不需要看圖說話，因為圖表自己會說話。"
    , Lesson "資料分析" [R] 3 3 [m,s] []
        "資料的表示法，決定了資料的意義。"
    , Lesson "圖象秩序" [V] 3 3 [a,m] []
        "一目了然。"
    , Lesson "戲劇社" [A,K] 4 4 [a,p] []
        "人只能活一次，戲裡，人可以活千萬次。"
    , Lesson "籃球社" [K,V] 4 4 [p,s] []
        "願你們的生命，像籃球一般，有清楚的目標。無論摔得多重，都會再彈起來。"
    , Lesson "寫作工坊" [A,R] 4 4 [a,c] []
        "工坊結合了創作的技藝，與心靈的交流。"
    , Lesson "經典研讀" [R] 6 6 [c] []
        "看一堆二手解說，不如直接去讀原典。"
    , Lesson "會話教學" [A] 6 6 [e] []
        "這堂課，只淮說英語。"
    , Lesson "體能集訓" [K] 6 6 [p] []
        "成功有三大秘訣。一：苦練，二：苦練，三：苦練。"
    , Lesson "情境教學" [A,K] 3 3 [a,e] [u]
        "身歷其境。"
    , Lesson "山訓" [K,V] 3 3 [n,p] [u]
        "我們爬的是不同的山。"
    , Lesson "詩作賞析" [A,R] 3 3 [c,e] [u]
        "詩是文字的結晶。"
    , Lesson "科學史" [A,R] 4 4 [m,n,s] []
        "工具是發明來解決問題，而不是製造麻煩的。掌握原始問題，就掌握了知識的大半。"
    , Lesson "語文史" [A,R] 4 4 [c,e,s] []
        "博古而能通今。"
    , Lesson "藝術史" [A,R] 4 4 [a,s] []
        "曾經，有人這樣嘗試過。"
   ]

skills =
    [ Skill "幽默" "無興趣時，蒙昧值採左右平均計算。"
        "他們喜歡的是學問本身，抑或只是課堂的遊戲活動？"
    , Skill "以火點火" "教學額外具有點燃興趣之能力。"
        "千年暗室，一燈即明。"
    , Skill "以心傳心" "即使學習風格不合，教學仍視為成功。"
        "教外別傳，不立文字。"
    , Skill "巧思" "教學卡皆獲得 +1 力道。"
        "尺有所短，寸有所長。"
    , Skill "圖象" "教學額外具有視覺風格。"
        "做為秩序的載體，圖象比文字更鮮明。"
    , Skill "實作" "教學額外具有操作風格。"
        "學以致用，從做中學。"
    , Skill "同理" "教學額外具有 ✙ 能力。"
        "解除小孩的麻痺經驗前，要先解除自己的麻痺經驗。"
    , Skill "敏銳" "只需 1 ⏎ 即可觀察學生。"
        "讓小孩教你怎樣教他。"
    , Skill "敏銳" "只需 1 ⏎ 即可觀察學生。"
        "先放下著急與成見。"
    , Skill "親和" "親和骰擲出 ⚁ 亦可成功。"
        "先為益友，後成良師。"
    , Skill "親和" "親和骰擲出 ⚁ 亦可成功。"
        "先為良師，後成益友。"
    , Skill "專業" "額外壓印一張教學卡：對此卡上每一學門，教學力道 +4。"
        "教學的技巧比較好練，學門的專業則需苦工。"
    , Skill "專業" "額外壓印一張教學卡：對此卡上每一學門，教學力道 +4。"
        "豐富的知識是體，教學的技巧是用。兩者缺一不可。"
   ]

environments =
    [ Environment "獨立教育工作者社群" "所有人的技藝成為共享的環境卡。"
        "這是追求成長的必然結果。"
    , Environment "師生人格平等" "所有人的親和骰點數 +1。"
        "教育是生命對生命的共鳴，師與生的角色只是表面。"
   ]

assistants =
    [ Assistant "萬事通" [A,R] [a,p] [] 1
        "光靠讀書，也能學會游泳喔。"
    , Assistant "小畫家" [V] [c,e,s] [] 1
        "這樣看，不就很明顯了嗎？"
    , Assistant "實驗鼠" [K] [m,n] [] 1
        "再試一下。"
    , Assistant "妙博士" [] [] [i] 4
        "它很美妙，它真的很美妙。"
   ]
