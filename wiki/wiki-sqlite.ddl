drop table if exists page;
create table page (
	id integer primary key autoincrement not null,
	title char(50) unique not null,
	text text
);

