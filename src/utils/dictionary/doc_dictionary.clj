(ns utils.dictionary.doc-dictionary
  (:require [utils.x.core :as u]
            [utils.dictionary.monodict :as mdict] 
            [utils.dictionary.bidict   :as bdict] 
            ))

(defn make-dict []
  {:loc-dict  (mdict/make-dict)
   :word-dict (bdict/make-dict)
   })

(defn lookup-first [d loc word]
  (let [lookup (mdict/lookup-first (:loc-dict d) loc)]
    (if (not (nil? lookup))
      lookup
      (let [lookup (bdict/lookup-first (:word-dict d) word)]
        lookup))))

(defn add-word [d loc word trans]
  (let [new-loc-dict  (mdict/add-word  (:loc-dict d) loc trans)
        new-word-dict (bdict/add-word (:word-dict d) word trans)]
    (assoc d 
           :loc-dict  new-loc-dict
           :word-dict new-word-dict)))

(defn add-words-lol [d [[loc word trans] & others]]
  (let [new-d (add-word d loc word trans)]
    (if (nil? others)
      new-d
      (recur new-d others))))

(defn add-words-flat [d [loc word trans & others]]
  (let [new-d (add-word d loc word trans)]
    (if (nil? others)
      new-d
      (recur new-d others))))
  
(def data_en_fr [["770:778","February","février"],["758:761","the","le"],["790:798","look-out","vigie"],["799:801","at","à"],["802:812","Notre-Dame","notre-dame"],["786:789","the","le"],["825:834","signalled","signala"],["839:851","three-master","trois-mâts"],["816:818","la","the"],["819:824","garde","garde"],["865:869","from","de"]])
(def data_fr_en [["1611:1616","vigie","look-out"],["1651:1653","le","the"],["1654:1664","trois-mâts","three-master"],["1665:1667","le","the"],["1617:1619","de","of"],["1679:1685","venant","coming"],["1686:1688","de","from"],["1689:1695","smyrne","smyrne"],["1697:1704","trieste","trieste"],["1708:1714","naples","naples"],["1705:1707","et","and"],["1717:1722","comme","as"],["1723:1733","d'habitude","usual"],["1735:1737","un","a"],["1738:1744","pilote","pilot"],["1745:1751","côtier","coastal"],["1752:1758","partit","left"],["1759:1767","aussitôt","immediately"],["1768:1770","du","from the"],["1771:1775","port","port"],["1785:1792","château","chateau"],["1793:1797","d'if","of if"],["1802:1806","alla","went"],["1807:1814","aborder","to meet"],["1818:1824","navire","boat"],["1825:1830","entre","between"],["1834:1837","cap","cape"],["1841:1848","morgion","morgion"],["1852:1857","l'île","the island"],["1858:1860","de","of"],["1861:1865","rion","rion"],["1895:1901","encore","still"],["1921:1925","fort","fort"],["1926:1936","saint-jean","saint-jean"],["1937:1944","s'était","was"],["1945:1953","couverte","covered"],["1954:1956","de","with"],["1957:1964","curieux","curious  (people)"],["1966:1969","car","because"],["1970:1975","c'est","it is"],["1976:1984","toujours","always"],["1985:1988","une","a"],["1989:1995","grande","big"],["1996:2003","affaire","event"],["2004:2005","à","at"],["2006:2015","marseille","marseilles"],["2020:2029","l'arrivée","the arrival"],["2030:2034","d'un","of a"],["2035:2043","bâtiment","ship"],["2045:2052","surtout","above all"],["2053:2058","quand","when"],["2059:2061","ce","this"],["2072:2077","comme","like"],["2094:2097","été","been"],["2098:2107","construit","built"],["2122:2125","sur","on"],["2126:2129","les","the"],["2130:2139","chantiers","building sites"],["2140:2142","de","of"],["2146:2153","vieille","old"],["2154:2160","phocée","phocee"],["2165:2175","appartient","belongs"],["2176:2177","à","to"],["2178:2180","un","an"],["2181:2189","armateur","owner"],["2196:2201","ville","city"],["2204:2213","cependant","however"],["2226:2236","s'avançait","advanced"],["2238:2240","il","it"],["2241:2246","avait","had"],["2247:2259","heureusement","happily"],["2260:2267","franchi","passed"],["2271:2278","détroit","strait"],["2279:2282","que","that"],["2283:2290","quelque","some"],["2291:2299","secousse","shock"],["2300:2310","volcanique","volcanic"],["2313:2319","creusé","dug"],["2335:2347","calasareigne","calasareigne"],["2360:2365","jaros","jaros"],["2376:2382","doublé","doubled"],["2383:2390","pomègue","pomegue"],["2409:2413","sous","under"],["2414:2417","ses","its"],["2418:2423","trois","three"],["2433:2436","son","its"],["2465:2469","mais","but"],["2470:2472","si","so"],["2473:2482","lentement","slowly"],["2486:2491","d'une","with an"],["2492:2498","allure","manner"],["2502:2508","triste","sad"],["2527:2531","avec","with"],["2532:2535","cet","that"],["2536:2544","instinct","instinct"],["2545:2548","qui","which"],["2549:2557","pressent","foretells"],["2558:2560","un","a"],["2561:2568","malheur","misfortune"],["2570:2572","se","themselves"],["2573:2584","demandaient","asked"],["2585:2589","quel","what"],["2590:2598","accident","accident"],["2599:2606","pouvait","could"],["2607:2611","être","be"],["2612:2618","arrivé","arrived"],["2619:2620","à","on"],["2621:2625","bord","board"],["2627:2636","néanmoins","nevertheless"],["2641:2648","experts","experts"],["2649:2651","en","in"],["2652:2662","navigation","navigation"],["2663:2678","reconnaissaient","recognised"],["2683:2685","si","if"],["2686:2688","un","an"],["2698:2703","était","was"],["2715:2717","ne","not"],["2731:2733","au","to the"],["2743:2751","lui-même","itself"],["2771:2775","dans","in"],["2776:2782","toutes","all"],["2787:2797","conditions","conditions"],["2810:2822","parfaitement","perfectly"],["2823:2831","gouverné","gouverned"]])

(defn test-dict []
  (let [d (add-words-lol {} data_en_fr)]
    (lookup-first d nil "February")))

(defn test-add []
  (let [d (add-words-flat {}
                     ["1" "hello" "bonjour"
                      "2" "hello" "bonjour"
                      "3" "hello" "bonjour1"
                      "4" "hello" "bonjour1"
                      "5" "hello" "bonjour1"
                      "6" "hello" "salut"
                      "7" "good" "bon"])]
    (mdict/lookup-first (:loc-dict d) "1")
    
    ))
