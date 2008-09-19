alter table albums modify titlesort varchar(255);
alter table albums add index(titlesort, disc);
alter table contributors modify namesort varchar(255);
