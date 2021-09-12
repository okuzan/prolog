book(code(43432),author('Тарас Шевченко'), 
	name('Кобзар'), publish_date(1840), publish_house('N/A')).

book(code(43431),author('Peterson Jordan'), 
	name('Rules for life'), publish_date(2012), publish_house('Penguin')).

book(code(48948),author('Leo Tolsoy'), 
	name('Anna Karenina'), publish_date(1821), publish_house('Penguin')).

book(code(23432),author('Unknown'), 
	name('Ukraine through ages'), publish_date(2022), publish_house('KOSMO')).

book(code(40432),author('Unknown'), 
	name('Ukraine through ages'), publish_date(2022), publish_house('KOSMO')).

book(code(40832),author('Moses'), 
	name('Exodus'), publish_date(1990), publish_house('Community')).

book(code(34389),author('Arthur Conan Doyle'), 
	name('Sherlock Holmes'), publish_date(1890), publish_house('Penguin')).


card(name('Кузан Олег Орестович'), code(48923), date_1(18, 10, 2020), date_2(27,10,2020), no).
card(name('Сергій Степанович'), code(46923), date_1(10, 10, 2020), date_2(27,11,2020), no).
card(name('Magnus Carlsen'), code(48925), date_1(19, 2, 2020), date_2(23, 2,2020), yes).
card(name('Ogel Furrough'), code(22225), date_1(29, 4, 2020), date_2(5, 5,2020), yes).


reader(name('Сергій Степанович'), address('Київ, Марини Цвєтаєвої 14-Б'),
	phone('09737256137'), age(33), occupation('officer')).

reader(name('Magnus Carlsen'), address('Lviv, V. Velykoho 180'),
	phone('06637256137'), age(30), occupation('janitor')).

reader(name('Кузан Олег Орестович'), address('Київ, Марини Цвєтаєвої 14-Б'),
	phone('09837456137'), age(20), occupation('student')).

reader(name('Ogel Furrough'), address('Dnipro, Shosse 2'),
	phone('09837456137'), age(25), occupation('finansist')).

% боржники
mortgager :=  card(_,_, _,_, no)
