drop table if exists page;
create table page (
	id int primary key not null auto_increment,
	title char(50) unique not null,
	text text
);

