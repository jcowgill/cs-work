-- SQL Problems
--  James Cowgill

-- Emp(empno, ename, hiredate, deptno, job, sal, comm, mgr)
-- Dept(deptno, dname, loc)

/*
CREATE TABLE `Dept` (
  `deptno` int(11) NOT NULL AUTO_INCREMENT,
  `dname` varchar(30) NOT NULL,
  `loc` varchar(30) NOT NULL,
  PRIMARY KEY (`deptno`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

CREATE TABLE `Emp` (
  `empno` int(11) NOT NULL AUTO_INCREMENT,
  `ename` varchar(30) NOT NULL,
  `hiredate` date NOT NULL,
  `deptno` int(11) NOT NULL,
  `job` enum('ANALYST','CLERK','MANAGER') NOT NULL,
  `sal` decimal(7,2) NOT NULL,
  `comm` decimal(7,2) NOT NULL,
  `mgr` int(11) DEFAULT NULL,
  PRIMARY KEY (`empno`),
  KEY `mgr` (`mgr`),
  CONSTRAINT `Emp_ibfk_1` FOREIGN KEY (`mgr`) REFERENCES `Emp` (`empno`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
*/

-- ################
-- Question 3
-- ################

-- 1) Display all information in the tables EMP and DEPT
SELECT * FROM Emp;
SELECT * FROM Dept;

-- 2) Display only the hire date and employee name for each employee
SELECT hiredate, ename FROM Emp;

-- 3) Display the hire date, name and department number for all clerks
SELECT hiredate, ename, deptno
FROM Emp
WHERE job = 'CLERK';

-- 4) Display the names and salaries of all employees with a salary greater than 2000
SELECT ename, sal
FROM Emp
WHERE sal > 2000;

-- 5) Display the names of all employees with an `A' in their name
SELECT ename
FROM Emp
WHERE ename LIKE '%A%';

-- 6) Display the names of all employees with exactly 5 letters in their name
SELECT ename
FROM Emp
WHERE ename LIKE '_____';

-- 7) Display the names and hire dates of all employees hired in 1981 or 1982
SELECT ename, hiredate FROM Emp
WHERE hiredate >= '1981-01-01' AND hiredate <= '1982-12-31';

-- 8) Display the names and dates of employees with the column headers "Name" and "Start Date"
SELECT ename AS 'Name', hiredate As 'Start Date'
FROM Emp;

-- 9) Display the names and hire dates of all employees in the order they were hired
SELECT ename, hiredate
FROM Emp
ORDER BY hiredate;

-- 10) Display the names and salaries of all employees in reverse salary order
SELECT ename, sal
FROM Emp
ORDER BY sal DESC;

-- 11) Display the department numbers of all departments employing a clerk
SELECT DISTINCT deptno
FROM Emp
WHERE job = 'CLERK';

-- ################
-- Question 4
-- ################

-- 1) Display the maximum, minimum and average salary and commission earned
SELECT MAX(sal), MIN(sal), AVG(sal), MAX(comm), MIN(comm), AVG(comm)
FROM Emp;

-- 2) Display the department number, total salary payout and total commission payout for each department
SELECT deptno, SUM(sal), SUM(comm)
FROM Emp
GROUP BY deptno;

-- 3) Display the department number, total salary payout and total commission payout for each department that pays at least one employee commission
SELECT deptno, SUM(sal), SUM(comm)
FROM Emp
GROUP BY deptno
HAVING SUM(comm) > 0;

-- 4) Display the department number and number of clerks in each department
SELECT deptno, COUNT(*) AS NumClerks
FROM Emp
WHERE job = 'CLERK'
GROUP BY deptno;

-- 5) Display the department number and total salary of employees in each department that employs four or more people
SELECT deptno, SUM(sal)
FROM Emp
GROUP BY deptno
HAVING COUNT(*) >= 4;

-- 6) Display the employee number of each employee who manages other employees with the number of people he or she manages
SELECT mgr, COUNT(*)
FROM Emp
WHERE mgr IS NOT NULL
GROUP BY mgr;

-- ################
-- Question 5
-- ################

-- 1) Display the name of each employee with his department name
SELECT ename, dname
FROM Emp JOIN Dept USING(deptno);

-- 2) Display a list of all departments with the employees in each department
SELECT dname, COUNT(empno)
FROM Emp RIGHT JOIN Dept USING(deptno)
GROUP BY deptno;

-- 3) Display all the departments with the manager for that department
SELECT dname, ename
FROM Emp JOIN Dept USING(deptno)
WHERE job = 'MANAGER';

-- 4) Display the names of each employee with the name of his/her boss
SELECT me.ename, boss.ename as bossname
FROM (Emp AS me) JOIN (Emp as boss) ON (boss.empno = me.mgr);

-- 5) Display the names of each employee with the name of his/her boss with a
--    blank for the boss of the president
SELECT me.ename, boss.ename as bossname
FROM (Emp AS me) LEFT JOIN (Emp as boss) ON (boss.empno = me.mgr);

-- 6) Display the employee number and name of each employee who manages other employees with the number of people he or she manages
SELECT boss.empno, boss.ename, COUNT(*) AS employees
FROM (Emp AS me) JOIN (Emp as boss) ON (boss.empno = me.mgr)
GROUP BY boss.empno;

-- 7) Repeat the display for the last question, but this time display the rows
--    in descending order of the number of employees managed
SELECT boss.empno, boss.ename, COUNT(*) AS employees
FROM (Emp AS me) JOIN (Emp as boss) ON (boss.empno = me.mgr)
GROUP BY boss.empno
ORDER BY employees DESC;

-- ################
-- Question 6
-- ################

-- 1) Display the names and job titles of all employees with the same job as Jones
SELECT ename, job
FROM Emp
WHERE job IN (SELECT job FROM Emp WHERE ename = 'Jones');

-- 2) Display the names and department name of all employees working in the same
--    city as Jones
SELECT ename, dname
FROM Emp JOIN Dept USING(deptno)
WHERE loc IN (SELECT loc
              FROM Emp JOIN Dept USING(deptno)
              WHERE ename = 'Jones');

-- 3) Display the name of the employee whose salary is the lowest
SELECT ename
FROM Emp
WHERE sal IN (SELECT MIN(sal) FROM Emp);

-- 4) Display the names of all employees except the lowest paid
SELECT ename
FROM Emp
WHERE sal NOT IN (SELECT MIN(sal) FROM Emp);

-- 5) Display the names of all employees whose job title is the same as anyone
--    in the sales dept
SELECT ename
FROM Emp
WHERE job IN (SELECT job
              FROM Emp JOIN Dept USING(deptno)
              WHERE dname = 'Sales');

-- 6) Display the names of all employees who work in a department that employs
--    an analyst
SELECT ename
FROM Emp
WHERE deptno IN (SELECT deptno
                 FROM Emp JOIN Dept USING(deptno)
                 WHERE job = 'ANALYST');

-- 7) Display the names of all employees with their job title, their current
--    salary and their salary following a 10% pay raise for clerks and a 7% pay
--    raise for all other employees
SELECT ename, job, sal, (sal * 1.10) AS payrise
    FROM Emp
    WHERE job = 'CLERK'
UNION ALL
SELECT ename, job, sal, (sal * 1.07) AS payrise
    FROM Emp
    WHERE job <> 'CLERK';

-- 8) Display the names of all employees with their salary and commission earned
--    Employees with a null commission should have 0 in the commission column
SELECT ename, sal, COALESCE(comm, 0)
FROM Emp;

-- 9) Display the names of ALL employees with the total they have earned
--    (i.e. salary plus commission)
SELECT ename, sal + comm
FROM Emp;

-- 10) Repeat the display for the last question but this time display in
--    descending order of earnings
SELECT ename, sal + comm AS total
FROM Emp
ORDER BY total DESC;
