
Select *
From COVIDProject..Deaths
Where continent is not null 
order by 3,4

--Select Data that we are going to be using

Select Location, date, total_cases, new_cases, total_deaths, population
From COVIDProject..Deaths
Where continent is not null
order by 1,2

--Looking at Total Cases vs Total Deaths
-- Shows likelihood of dying if you contract covid in the united states
Select Location, date, total_cases, total_deaths,  (cast(total_deaths as float)/cast(total_cases as float))*100 as DeathPercentage
From COVIDProject..Deaths
Where continent is not null
order by 1,2

--Looking at Total Cases vs. Population

Select Location, date, Population, total_cases, (cast(total_cases as float)/cast(population as float))*100 as PercentofPopulationInfected
From COVIDProject..Deaths
Where continent is not null
order by 1,2

--Countries with highest infection rate vs population

Select Location, Population, MAX(total_cases) as HighestInfectionCount, MAX((cast(total_cases as float)/cast(population as float)))*100 as PercentofPopulationInfected
From COVIDProject..Deaths
Where continent is not null
Group by Location, Population
order by PercentofPopulationInfected desc

--Countires with Highest Death Count per Population

Select Location, MAX(cast(total_deaths as int)) as TotalDeathCount
From COVIDProject..Deaths
Where continent is not null
Group by Location, Population
order by TotalDeathCount desc

--BROKEN DOWN BY CONTINENT

Select continent, MAX(cast(total_deaths as int)) as TotalDeathCount
From COVIDProject..Deaths
Where continent is not null
Group by continent
order by TotalDeathCount desc

--Continents with the Highest Death Count per Population

Select continent, MAX(cast(total_deaths as int)) as TotalDeathCount
From COVIDProject..Deaths
Where continent is not null
Group by continent
order by TotalDeathCount desc

--Global Numbers

Select SUM(new_cases) as total_cases, SUM(cast(new_deaths as int)) as total_deaths, SUM(cast(new_deaths as int))/NULLIF(SUM(cast(New_cases as float)),0)*100 as DeathPercentage
From COVIDProject..Deaths
Where continent is not null
order by 1,2

--Total Population vs Vaccinations

Select dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations
, SUM(CONVERT(bigint,vac.new_vaccinations)) OVER (Partition by dea.Location Order by dea.location, dea.date) as RollingPeopleVaccinated
From COVIDProject..Deaths dea
Join COVIDProject..Vaccinations vac
	On dea.location = vac.location
	and dea.date = vac.date
where dea.continent is not null
order by 2,3

--CTE

With PopvsVac (Continent, Location, Date, Population, New_Vaccinations, RollingPeopleVaccinated)
as
(
Select dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations
, SUM(CONVERT(bigint,vac.new_vaccinations)) OVER (Partition by dea.Location Order by dea.location, dea.date) as RollingPeopleVaccinated
From COVIDProject..Deaths dea
Join COVIDProject..Vaccinations vac
	On dea.location = vac.location
	and dea.date = vac.date
where dea.continent is not null
)
Select *, (RollingPeopleVaccinated/Population)*100
From PopvsVac

--Creating View to store data for visualizations

Create View PercentPopulationVaccinated as 
Select dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations
, SUM(CONVERT(bigint,vac.new_vaccinations)) OVER (Partition by dea.Location Order by dea.location, dea.date) as RollingPeopleVaccinated
From COVIDProject..Deaths dea
Join COVIDProject..Vaccinations vac
	On dea.location = vac.location
	and dea.date = vac.date
where dea.continent is not null
