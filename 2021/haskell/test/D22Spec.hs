module D22Spec (spec) where

import Test.Hspec
import Days.D22


spec :: Spec
spec = do
  let input = [(True,((-45,7),(-17,27),(5,49))),(True,((-47,6),(-17,30),(-24,26))),(True,((-41,3),(-7,39),(-40,13))),(True,((-7,37),(-13,39),(-8,42))),(True,((-28,25),(1,47),(-30,16))),(True,((-17,35),(-41,8),(-24,28))),(True,((-12,40),(-16,36),(-9,38))),(True,((-42,5),(-43,5),(-41,5))),(True,((-41,8),(-19,32),(-5,48))),(True,((-7,45),(-48,-4),(-36,18))),(False,((-30,-17),(-18,-5),(14,31))),(True,((-34,11),(-2,43),(-40,12))),(False,((15,31),(8,23),(-25,-14))),(True,((1,49),(-7,39),(-21,27))),(False,((-10,0),(24,33),(-25,-8))),(True,((-45,1),(-5,45),(-20,31))),(False,((-45,-28),(-3,15),(29,43))),(True,((-28,22),(-13,33),(-17,31))),(False,((40,49),(9,24),(-38,-21))),(True,((-31,20),(-21,31),(-10,40))),(True,((58139,70015),(-49609,-18906),(19361,36256))),(True,((-39287,-12309),(66131,82072),(13276,35310))),(True,((-38949,-17453),(-49529,-24823),(55665,59650))),(True,((61392,82266),(-12404,6500),(4950,31263))),(True,((70741,83016),(-1541,20873),(-21674,7357))),(True,((-34218,162),(-70012,-44525),(42760,66985))),(True,((-47728,-21420),(-1708,17042),(-84807,-57400))),(True,((-69820,-59219),(43821,69305),(2296,22054))),(True,((46547,61145),(-50265,-36252),(19460,47328))),(True,((10220,47534),(56556,93325),(-2705,20946))),(True,((-66580,-44842),(-59481,-31671),(18352,40065))),(True,((-67971,-41858),(-15159,3388),(39731,56500))),(True,((-7464,2686),(69347,76538),(13667,39722))),(True,((-44072,-16433),(-43849,-30853),(56335,69595))),(True,((-83105,-72178),(5307,31541),(-3936,16810))),(True,((25464,39638),(-120,31814),(-69618,-52103))),(True,((-82616,-57987),(15114,25223),(-28861,-23141))),(True,((-49633,-29006),(41204,52341),(-65156,-47964))),(True,((41031,61766),(-76189,-58486),(9912,27403))),(True,((57437,69980),(12769,38313),(-43090,-17897))),(True,((17604,33962),(-75729,-58412),(-48962,-35687))),(True,((63409,77341),(-21036,12184),(29223,43584))),(True,((-5980,19717),(41947,72255),(38283,56970))),(True,((9718,12557),(67771,85749),(17773,34267))),(True,((8015,28022),(-18543,-3527),(-89441,-59883))),(True,((-64308,-56323),(21361,38767),(12257,42684))),(True,((-11882,16043),(30052,57566),(57092,81130))),(True,((-13820,53),(-74086,-63407),(37807,48599))),(True,((40956,59336),(58187,69842),(25877,39207))),(True,((-4691,23405),(-79041,-70404),(-14086,5935))),(True,((66930,81266),(-37722,-4197),(27732,37351))),(True,((-19848,-2104),(61123,83113),(11320,30475))),(True,((-66891,-46977),(-588,33596),(33866,52493))),(True,((-13090,15764),(63984,75318),(23277,60045))),(True,((-14119,15900),(-13901,1426),(60017,94903))),(True,((12498,29184),(58541,70777),(-50866,-38397))),(True,((-67009,-43569),(-69320,-39097),(24026,51765))),(True,((17594,36756),(25770,47449),(47460,61914))),(True,((43517,53046),(-56173,-37227),(20953,36522))),(True,((39963,42250),(57583,75800),(-1394,6811))),(True,((19518,48960),(52233,66739),(-48141,-30882))),(True,((52411,62263),(-70027,-49483),(516,15609))),(True,((-27930,-5097),(-74677,-52111),(-59808,-41624))),(True,((56383,62339),(-62443,-42931),(7509,24599))),(True,((43191,61390),(36457,58193),(-16838,1525))),(True,((32499,55237),(-69168,-42561),(-10459,-1917))),(True,((-58208,-37739),(-28039,-4075),(40414,61838))),(True,((879,22641),(74295,88511),(-19483,9324))),(True,((-5492,8717),(-85892,-60796),(-41420,-29271))),(True,((-61450,-39696),(-49010,-38395),(21478,40987))),(True,((32490,45756),(-8722,-4698),(-76184,-56133))),(True,((-90953,-70436),(-29377,-3902),(-40397,-23576))),(True,((20598,38603),(66163,85567),(9851,28068))),(True,((27571,32595),(-59796,-30762),(-63579,-38559))),(True,((12086,41959),(50402,75453),(-54908,-21406))),(True,((-14331,1008),(71841,90790),(-2133,19891))),(True,((3915,28155),(-5795,4405),(75839,94136))),(True,((-55589,-36558),(47040,66883),(-30804,-10538))),(True,((-82372,-51132),(-47601,-21545),(-40952,-21789))),(True,((-231,19231),(-59449,-29763),(56397,73100))),(True,((-85290,-58709),(-16609,-8329),(-29163,4167))),(True,((-63346,-48051),(52873,69585),(-30842,3080))),(True,((-25328,-4496),(12591,44061),(-84403,-55728))),(True,((-21682,13927),(-80747,-59930),(-5486,20947))),(True,((8693,18060),(4143,13917),(72062,87750))),(True,((-58360,-31194),(47907,73246),(8480,21809))),(True,((2105,34277),(52967,69971),(-50821,-20714))),(True,((22406,39629),(50181,61542),(43485,55870))),(True,((-39074,-13502),(50579,72036),(-60260,-46008))),(True,((-16311,-6287),(16984,36635),(68924,86294))),(True,((-48550,-15024),(-61029,-31290),(54895,67840))),(True,((-50877,-30410),(-1926,13529),(53814,85287))),(True,((-36471,-13748),(36383,48289),(-83446,-48216))),(True,((26766,48259),(-43762,-25681),(56822,77314))),(True,((-17837,10480),(-63530,-38447),(-59572,-41895))),(True,((20337,49911),(55663,87453),(5287,22885))),(True,((-55628,-21884),(37700,65006),(38798,55418))),(True,((-67405,-42145),(-7082,13919),(-63850,-39429))),(True,((-62190,-49213),(49865,67498),(3390,13755))),(True,((-51622,-18963),(51212,74783),(-28793,-8944))),(True,((66722,82965),(5925,20106),(28397,44234))),(True,((-66827,-51821),(2821,11144),(30591,50432))),(True,((35642,66682),(-74513,-45994),(-15026,5100))),(True,((27277,36975),(-73319,-45025),(-54816,-32826))),(True,((17935,54757),(-61336,-35653),(55321,68784))),(True,((-53245,-32593),(-8830,8348),(-76698,-59749))),(True,((-43668,-18711),(49769,78016),(-50596,-28488))),(True,((-72259,-40599),(40207,59440),(21612,41506))),(True,((50424,74664),(24414,39823),(-38827,-9157))),(True,((-58443,-41638),(19203,40243),(-54425,-48848))),(True,((-55245,-32288),(56063,70391),(-61296,-37638))),(True,((-49262,-36162),(40765,56914),(39896,58492))),(True,((71936,78823),(21562,36332),(-6530,9437))),(True,((21183,33209),(-63451,-35210),(43680,67677))),(True,((35315,59459),(-79781,-55078),(-5718,9678))),(True,((5598,19316),(62740,70153),(-38729,-30069))),(True,((59043,77670),(-53992,-36698),(-18798,1018))),(True,((32561,52959),(54567,76299),(-6363,11619))),(True,((64072,83686),(19684,46937),(-26206,-226))),(True,((66407,80678),(-6031,15815),(-50231,-28853))),(True,((-62433,-51106),(-50185,-42513),(-59329,-34248))),(True,((69442,84353),(-37609,-18888),(15039,22599))),(True,((-75863,-45632),(-67797,-44088),(13448,35319))),(True,((-57525,-34978),(49512,61488),(-34514,-10529))),(True,((-78241,-45455),(-41939,-25787),(-37581,-12052))),(True,((26231,48086),(55721,87000),(-26989,-2861))),(True,((-4820,16896),(-89410,-59766),(-12799,17397))),(True,((-65173,-53273),(33923,60231),(22787,35885))),(True,((-25789,-12467),(-20692,-1528),(-90659,-77680))),(True,((-13681,4235),(-72843,-65344),(37936,54871))),(True,((-11,15471),(-77300,-67134),(30860,37972))),(True,((-28323,2329),(43128,57045),(40410,68197))),(True,((-77368,-60587),(-40892,-36302),(-4933,14472))),(True,((-71440,-43438),(29241,61084),(-45200,-17748))),(True,((-46541,-29430),(-30481,-20386),(-77000,-48239))),(True,((-7222,17417),(-82920,-65369),(-24777,-2228))),(True,((2428,23021),(49309,71893),(33651,47624))),(True,((-79286,-73434),(-4555,3976),(-30692,-15990))),(True,((-57833,-36691),(50060,70993),(-49705,-40886))),(True,((-13633,-3901),(70191,89145),(-9546,8579))),(True,((30203,51142),(41221,64139),(-49984,-36750))),(True,((25779,40661),(62196,84217),(3820,31889))),(True,((-58668,-46042),(46561,68990),(-29470,-12201))),(True,((8943,46059),(-61596,-28191),(45500,78129))),(True,((28768,53964),(-39042,-2958),(61006,67475))),(True,((-10473,2243),(-57135,-35805),(50664,61840))),(True,((63395,88485),(-28148,-8518),(-20034,6196))),(True,((6817,17829),(-22295,-6626),(60702,92502))),(True,((-54667,-43405),(-35771,-14244),(-62194,-47623))),(True,((18751,34299),(44719,66964),(-65540,-32700))),(True,((-24683,-8536),(-57872,-22245),(57265,75481))),(True,((-62653,-47058),(-71546,-56371),(4383,22163))),(True,((47349,60320),(54753,74576),(-21348,2434))),(True,((-38274,-12869),(-7578,3906),(-87883,-58190))),(True,((-33826,4521),(-11790,8404),(-86733,-67748))),(True,((-52661,-35931),(26091,49288),(-66021,-48412))),(True,((44264,57460),(58198,70004),(2720,15895))),(True,((17474,36290),(67465,77999),(1861,17546))),(True,((-38975,-10327),(-27672,4843),(-83570,-68390))),(True,((-33746,-20547),(-74631,-45171),(39800,51056))),(True,((-24599,-21768),(-46393,-24475),(55208,71397))),(True,((3164,19967),(-65190,-44643),(50418,72570))),(True,((53619,66155),(-51057,-29658),(-55773,-20405))),(True,((2427,22904),(-95956,-66495),(-36877,-18825))),(True,((2944,9603),(-86498,-58478),(-47818,-30617))),(True,((-10384,6884),(38548,65953),(48033,75190))),(True,((41974,68250),(34200,53098),(30209,46591))),(True,((-90428,-64064),(-9949,-7541),(-23619,-456))),(True,((-66305,-49896),(12749,28681),(57529,59079))),(True,((-93786,-65183),(13210,20464),(22646,36054))),(True,((-71583,-55593),(-18075,-7073),(17505,49567))),(True,((-63215,-45228),(-11391,7580),(58194,80874))),(True,((-24085,-13987),(-74188,-54667),(40270,59993))),(True,((41063,66463),(-60593,-28582),(-57706,-22356))),(True,((-56785,-44700),(-53712,-30970),(27312,36091))),(True,((-14349,6107),(-5236,15914),(-89681,-61552))),(True,((-2414,15438),(69685,96612),(-17622,9238))),(True,((58420,63800),(5990,23102),(34891,71606))),(True,((6179,27982),(-74431,-54768),(14204,36485))),(True,((61894,69769),(34173,44733),(8902,19407))),(True,((-78362,-63040),(19103,50632),(-32334,-17303))),(True,((62182,90308),(-18030,8929),(-2180,13388))),(True,((30390,48405),(-72383,-48673),(36144,57755))),(True,((-45454,-20423),(-70458,-62117),(-21023,1613))),(True,((36175,58944),(-81657,-52999),(2448,22806))),(True,((54470,71193),(41067,44776),(25090,44500))),(True,((-91961,-67717),(3964,29558),(-28094,6750))),(True,((-31391,-9169),(-18878,6593),(-78341,-75988))),(True,((47896,79657),(-67744,-30097),(-24880,-16))),(True,((-64300,-45475),(7409,21358),(-57363,-47213))),(True,((-73942,-56617),(-35835,-23791),(-67498,-46695))),(True,((2869,23028),(36519,60117),(-54212,-41508))),(True,((2029,20149),(52897,82718),(28679,40476))),(True,((-36196,-4590),(-63991,-45779),(42563,79330))),(True,((14025,39832),(49218,57689),(-59867,-36377))),(True,((48031,55950),(52029,77629),(-8043,15625))),(True,((20246,41538),(-11202,9933),(72656,87842))),(True,((-67616,-40170),(12132,31220),(-74768,-55643))),(True,((-5329,848),(13794,51295),(-77787,-65482))),(True,((-75861,-64089),(-21592,-3946),(-39892,-8479))),(True,((-77211,-65745),(27088,38134),(-13374,-8928))),(True,((37830,58943),(47065,77746),(298,18874))),(True,((-75469,-41239),(-60573,-47270),(-8955,16006))),(True,((-56176,-39724),(-61947,-41061),(-68714,-31253))),(True,((-86282,-64173),(12949,39281),(24182,46117))),(True,((-41914,-22656),(-79945,-51310),(15689,35920))),(True,((21347,36914),(56858,86289),(19672,35772))),(True,((12201,29696),(59898,74244),(31480,58564))),(True,((23451,53913),(5070,36118),(-71964,-55710))),(True,((52084,63506),(2554,22599),(-55360,-32465))),(True,((13753,23490),(63510,88732),(4051,12664))),(True,((-69866,-44325),(36781,64233),(29840,36459))),(True,((-8139,-4203),(19157,40182),(-93960,-65960))),(True,((42305,64756),(22258,36563),(41947,60736))),(True,((-23146,10560),(-99141,-74322),(30,10375))),(True,((-67502,-44911),(-58624,-54469),(-20071,40))),(True,((-1816,30980),(-93210,-68893),(17460,29017))),(True,((-66971,-39649),(46207,60551),(-2186,20048))),(True,((57315,58674),(33730,47187),(-32958,-18355))),(True,((55047,84474),(24699,40492),(-46207,-20341))),(False,((-33063,-21256),(-57696,-30224),(-58827,-49473))),(True,((15782,43013),(-31004,-22072),(65339,89647))),(True,((-46864,-38602),(-59421,-34651),(-42145,-38505))),(True,((338,19140),(67761,75432),(28552,32028))),(True,((1165,3108),(-84103,-65417),(-7021,18773))),(False,((61765,73452),(-45659,-31483),(6273,20553))),(True,((-5904,9216),(-95279,-75566),(-15436,-7219))),(False,((32482,50315),(18574,37548),(55762,79297))),(False,((28578,60666),(-57164,-47836),(-37456,-18580))),(False,((-3114,3055),(-95936,-75112),(16370,29867))),(True,((-10868,13285),(-23894,-5902),(-93321,-72263))),(True,((-41991,-24954),(31908,51061),(-70741,-48428))),(True,((72292,78583),(13091,46604),(-16520,-8162))),(True,((-80845,-74837),(15441,28048),(-5348,4713))),(False,((51932,73538),(-55178,-32685),(-46650,-27464))),(True,((46484,64440),(22782,44759),(-50796,-20460))),(False,((-34434,-14835),(27486,46328),(56348,82832))),(True,((-44176,-28566),(-11705,15875),(56902,81288))),(True,((-33731,-11689),(73052,83023),(16583,38825))),(True,((6312,27150),(-793,26604),(-92489,-61569))),(False,((14092,28444),(66009,93462),(-21640,4954))),(True,((16209,38551),(-13562,-1358),(-88722,-74583))),(True,((-59972,-25520),(41806,72865),(-54134,-33465))),(True,((-61623,-33367),(-896,19881),(51101,71993))),(True,((-9583,14577),(-86565,-55523),(-51532,-34350))),(False,((-33213,-7919),(-90889,-66554),(-39490,-10974))),(True,((4294,20106),(62156,76620),(32450,55005))),(False,((-40471,-12398),(71892,92866),(-19239,3156))),(True,((61770,76100),(31826,33819),(-9830,20872))),(True,((-25575,-23040),(41833,59675),(44548,55450))),(False,((68252,71314),(-21177,2051),(31408,54792))),(False,((-8045,11292),(-72391,-63911),(33543,45802))),(True,((-8743,10578),(-32006,-6402),(-93482,-68214))),(False,((73561,91272),(5808,21229),(-9935,2763))),(True,((-30457,-8215),(68079,79651),(-10259,-6593))),(True,((47517,81726),(-2770,15448),(25263,47900))),(True,((54080,68182),(-23598,5989),(-59609,-46354))),(False,((-75813,-65614),(-15792,17698),(22522,37787))),(True,((-87006,-65845),(-33396,-12386),(5059,13493))),(False,((-7171,8455),(-92708,-74085),(13156,24440))),(True,((-10212,9937),(59394,87542),(-40866,-18151))),(False,((6150,43593),(-40711,-23001),(55695,76253))),(True,((-35948,-15354),(-62908,-59402),(-45479,-22686))),(True,((805,9285),(59136,71915),(-59038,-37416))),(True,((38933,59498),(49011,71006),(-8794,14206))),(True,((-60069,-28629),(21605,46948),(41655,66447))),(False,((-11597,14682),(-89638,-61313),(11985,34734))),(True,((49754,60626),(-10194,4245),(-57384,-56173))),(True,((-76993,-49823),(1985,21722),(27195,40273))),(True,((3697,32693),(39006,59117),(47166,67641))),(True,((-46344,-31390),(26350,43919),(-59943,-51289))),(False,((-61186,-23421),(3628,26108),(-75248,-54653))),(True,((-32256,3212),(65884,75652),(26282,48080))),(True,((24335,60592),(26877,56269),(-59010,-35566))),(True,((-47929,-11220),(61537,80705),(-951,12658))),(False,((-34766,-24336),(-88672,-68491),(-22764,-2566))),(False,((53419,59301),(43238,55188),(2622,33800))),(False,((-10211,14650),(-84660,-49473),(-60698,-24956))),(True,((-79252,-61788),(12398,35394),(1279,18242))),(True,((-7059,9529),(47864,69006),(49118,58161))),(False,((71189,88378),(30685,51825),(-9146,-3034))),(False,((-81189,-65062),(-10666,93),(-49110,-27410))),(True,((-32473,3746),(-64466,-43485),(32647,60505))),(False,((13789,47745),(25232,46055),(58850,67334))),(True,((36081,58769),(6844,15343),(53742,79111))),(True,((69079,94470),(-15594,4678),(3766,24355))),(False,((-67105,-49557),(-38660,-26297),(43118,59948))),(False,((50681,80664),(-19761,-8580),(-42615,-37383))),(False,((-47909,-18354),(49435,68877),(-43529,-15345))),(True,((-88967,-59205),(-13016,3451),(-54832,-19130))),(False,((10643,25625),(10545,27433),(64296,89575))),(False,((4907,32957),(-83127,-55966),(-39854,-32569))),(True,((22784,48884),(-57836,-21249),(-65327,-56473))),(False,((10992,29981),(66267,77315),(-41222,-18950))),(False,((-52091,-37868),(-20232,-474),(47891,72527))),(True,((60481,75081),(24637,41188),(-46716,-40883))),(False,((-23632,5610),(1514,22656),(74313,88576))),(False,((31575,38873),(40494,57318),(32877,56552))),(False,((24103,51366),(-54507,-36072),(-65355,-36367))),(False,((-25237,-3440),(-72086,-51759),(-59419,-40897))),(False,((-18141,16100),(11832,33275),(-87594,-71462))),(False,((12042,37177),(-86189,-74493),(-19781,-3966))),(True,((-12444,12639),(-85487,-65396),(-35640,-9225))),(False,((-61040,-36584),(-81216,-48830),(-14964,12506))),(False,((-23554,-6117),(-72205,-47980),(31059,61897))),(False,((65088,79105),(-37874,-16691),(3979,35678))),(False,((-53117,-33574),(-13523,-5076),(51420,78156))),(True,((-25476,-9172),(-82861,-63199),(-43771,-26786))),(True,((-86034,-56752),(6474,32237),(18690,35512))),(False,((21171,53995),(-36024,-8618),(-85718,-54617))),(True,((24027,48916),(30554,49211),(56592,63360))),(False,((-31460,-8363),(-91538,-67279),(-20883,11877))),(False,((50868,72728),(-43133,-20868),(30586,59496))),(True,((60180,77450),(-28264,-4028),(27566,46058))),(False,((25248,40253),(52738,82118),(-10027,10323))),(False,((-53149,-35544),(26124,57599),(41182,47863))),(True,((6893,26612),(-56766,-18582),(-76077,-64415))),(False,((29251,43365),(-56034,-33329),(-74917,-58876))),(False,((55417,84034),(13218,36893),(29426,38841))),(False,((15245,41851),(-51152,-25375),(51072,78544))),(False,((-19619,360),(49798,63770),(-68779,-57126))),(False,((48875,73188),(36354,65964),(20137,55472))),(True,((-50376,-23916),(-84993,-56907),(15202,30720))),(True,((-63177,-46387),(30846,55077),(19244,38186))),(True,((40274,55030),(-71784,-45007),(-4208,20470))),(True,((28347,49207),(63036,73385),(-17068,13520))),(False,((-26968,-3637),(-9695,16790),(66767,90772))),(True,((68705,98416),(-8622,10759),(3213,26868))),(False,((-3059,12360),(66783,80670),(-28986,7977))),(True,((-58388,-40646),(-33991,-31773),(44197,59752))),(True,((2865,11833),(-67250,-57894),(-62792,-24337))),(False,((-57030,-37005),(51218,75936),(7769,32000))),(True,((47527,74301),(-58273,-36741),(10584,15071))),(True,((-60184,-39571),(-30740,-20737),(-68300,-39656))),(False,((-86850,-65643),(-5574,16927),(3990,27771))),(False,((13811,46354),(-31173,3401),(-79763,-64407))),(False,((-13279,4029),(74369,84704),(-9402,6592))),(False,((-38919,-37515),(42906,53285),(33500,63457))),(False,((-3774,27927),(-52395,-36924),(50341,76959))),(False,((-86316,-65121),(-11814,7408),(-13538,17834))),(False,((39060,65192),(-49523,-12954),(44981,65013))),(True,((44289,52572),(5584,40317),(-73249,-41724))),(True,((41428,64314),(-52679,-24605),(-44071,-26710))),(True,((-46798,-20610),(-70188,-56150),(-53205,-16499))),(True,((-40671,-20938),(69319,88891),(5610,16726))),(False,((13893,35132),(-15329,7573),(-95287,-72786))),(True,((-31073,-26913),(-77563,-57760),(1928,22949))),(True,((-53823,-47132),(7432,14072),(-65922,-42630))),(False,((-92914,-60621),(-25654,5949),(12601,22146))),(False,((24459,62772),(61174,75406),(24143,41263))),(True,((-73652,-36824),(23996,28076),(-68072,-50254))),(False,((5695,26986),(-27324,-3742),(58054,83456))),(True,((30188,51914),(53458,67994),(5460,30526))),(False,((67145,80765),(-3482,25880),(29419,47511))),(False,((-89026,-70655),(-42051,-13350),(15265,35268))),(False,((-28593,-19842),(27064,45165),(57782,73792))),(True,((-46554,-23972),(33019,60152),(46108,61039))),(True,((-51229,-27692),(62079,81900),(-25204,-10313))),(False,((75830,79640),(-8232,15863),(12397,27598))),(False,((47654,68376),(351,18328),(-61385,-30558))),(False,((-49195,-23856),(57499,72955),(-5783,32067))),(False,((-4065,13165),(-53869,-18692),(-77892,-68369))),(True,((21771,41037),(33685,63960),(35189,54429))),(True,((-62848,-42784),(-53293,-32454),(-42889,-23060))),(True,((16633,41662),(-77505,-49183),(34223,52408))),(True,((41423,65193),(-74897,-45854),(-16083,15691))),(False,((5197,23912),(-2192,30686),(58695,84997))),(True,((51540,52711),(-68340,-53654),(-20777,-19220))),(True,((-48130,-28617),(-9415,-1637),(69241,89576))),(True,((2500,18895),(-56133,-49184),(52561,81563))),(False,((55700,81258),(14145,34650),(25325,46304))),(False,((-24233,-94),(-4089,20455),(75592,93976))),(False,((-73100,-65288),(-8770,2793),(-46595,-28483))),(False,((20475,36233),(65981,81822),(10098,30519))),(True,((-47992,-30588),(-64098,-33920),(48972,77171))),(True,((-13261,8398),(48870,70645),(54671,71340))),(True,((-70105,-48865),(35572,63927),(-16566,-3809))),(False,((-90578,-69121),(7582,45468),(-7064,10904))),(False,((-31215,-10949),(72290,79639),(-6286,6345))),(True,((74052,84728),(16884,29952),(-19273,9356))),(False,((35204,64806),(55000,69546),(-3598,9749))),(False,((-49474,-24068),(53139,55822),(-57619,-47785))),(False,((32313,52383),(-26381,-13630),(58768,73790))),(False,((13580,35467),(61719,88895),(8669,29467))),(True,((-54515,-49805),(-4119,23034),(-69401,-56491))),(True,((-73961,-54235),(25557,37602),(-59903,-33239))),(False,((-10313,2971),(-56537,-21479),(-73118,-56758))),(True,((-74109,-68245),(-14640,6255),(-32056,-9919))),(True,((-8704,1766),(35410,57231),(64345,76443))),(False,((73047,81163),(-5773,17096),(24433,32163))),(False,((-91886,-62451),(-482,26130),(8006,26319))),(False,((17921,36441),(-67251,-44885),(34231,42934))),(True,((49934,68358),(-53246,-19769),(39469,43842))),(True,((56532,66012),(-63433,-34360),(-5761,16747))),(False,((50888,66910),(-73766,-45524),(-34269,-15791))),(False,((-53699,-35241),(-53482,-17862),(-72515,-42474))),(False,((-48183,-29326),(46638,54456),(-54302,-33975))),(False,((53493,67034),(45776,48825),(2583,8620))),(True,((-6073,10138),(62545,85278),(23621,57001))),(True,((-38508,-4523),(54082,70881),(50826,67962))),(False,((37169,76620),(47581,59105),(-10872,8763))),(True,((-28636,-16986),(33405,60075),(44952,63962))),(True,((-8687,15348),(-83584,-62431),(-17101,-2245))),(False,((36300,56740),(46976,69120),(-8811,2238))),(False,((-5004,12191),(-40112,-21492),(-79188,-55268))),(True,((23083,34899),(2757,27461),(-80465,-53265))),(False,((36563,47394),(34869,51935),(-60753,-41403))),(False,((-65592,-49393),(-38929,-37743),(-52793,-22052))),(False,((-56455,-27816),(-57252,-39373),(-64915,-35370))),(False,((-55271,-22482),(36129,49610),(34155,56011))),(True,((-16603,-11223),(6299,34093),(-82645,-58961))),(True,((41076,70147),(7237,29376),(-49485,-30152))),(True,((535,30263),(-52689,-37336),(46273,84327))),(True,((-21910,3792),(22950,52814),(63693,76400))),(False,((-81116,-50991),(276,6719),(-61422,-30155))),(True,((70070,94722),(3705,21041),(-36356,-6622))),(True,((-58964,-32366),(-64952,-47587),(-392,17097))),(False,((10390,21102),(64923,80569),(34182,56673))),(True,((-45522,-28586),(-67635,-47968),(-6313,27037))),(True,((-18210,17089),(71004,89678),(-15469,10434)))]
  let p1Answer = 607657
  let p2Answer = 1187742789778677

  it "part1" $ do
    part1 input `shouldBe` p1Answer

  describe "part2" $ do
    it "part2" $ do part2u input `shouldBe` p2Answer
    it "part2 (opt)" $ do part2u' input `shouldBe` p2Answer
    it "part2 (opt2)" $ do part2u'' input `shouldBe` p2Answer
    it "part2 (intersect)" $ do part2i input `shouldBe` p2Answer
      