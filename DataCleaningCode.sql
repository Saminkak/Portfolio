--Data Cleaning in SQL 

Select *
From DataCleaningProject..HousingData

--Standardize Date Format (Invalid due to removed columns)

Select SaleDateConverted, CONVERT(Date,SaleDate)
From DataCleaningProject..HousingData

Update HousingData
SET SaleDate = CONVERT(Date,SaleDate)

ALTER TABLE DataCleaningProject..HousingData
Add SaleDateConverted Date;

Update DataCleaningProject..HousingData
SET SaleDateConverted = CONVERT(Date,SaleDate)

--Populate Property Address (Columns removed) 

Select PropertyAddress
From DataCleaningProject..HousingData
--Where PropertyAddress is null
order by ParcelID

Select a.ParcelID, a.PropertyAddress, b.ParcelID, b.PropertyAddress, ISNULL(a.PropertyAddress, b.PropertyAddress)
From DataCleaningProject..HousingData a
JOIN DataCleaningProject..HousingData b
	on a.ParcelID = b.ParcelID
	AND a.[UniqueID ] <> b.[UniqueID ]
Where a.PropertyAddress is null

Update a
SET PropertyAddress = ISNULL(a.PropertyAddress, b.PropertyAddress)
From DataCleaningProject..HousingData a
JOIN DataCleaningProject..HousingData b
	on a.ParcelID = b.ParcelID
	AND a.[UniqueID ] <> b.[UniqueID ]
Where a.PropertyAddress is null

--Dividing Address into Individual Columns 

Select PropertyAddress
From DataCleaningProject..HousingData
--Where PropertyAddress is null
--order by ParcelID

SELECT 
SUBSTRING(PropertyAddress, 1, CHARINDEX(',', PropertyAddress) -1) as Address
, SUBSTRING(PropertyAddress, CHARINDEX(',', PropertyAddress) +1, LEN(PropertyAddress)) as Address
From DataCleaningProject..HousingData

ALTER TABLE DataCleaningProject..HousingData
Add PropertySplitAddress Nvarchar(255);

Update DataCleaningProject..HousingData
SET PropertySplitAddress = SUBSTRING(PropertyAddress, 1, CHARINDEX(',', PropertyAddress) -1)

ALTER TABLE DataCleaningProject..HousingData
Add PropertySplitCity Nvarchar(255);

Update DataCleaningProject..HousingData
SET PropertySplitCity = SUBSTRING(PropertyAddress, CHARINDEX(',', PropertyAddress) +1, LEN(PropertyAddress))

Select*
From DataCleaningProject..HousingData

Select OwnerAddress
From DataCleaningProject..HousingData

Select
PARSENAME(REPLACE(OwnerAddress, ',', '.') , 3)
,PARSENAME(REPLACE(OwnerAddress, ',', '.') , 2)
,PARSENAME(REPLACE(OwnerAddress, ',', '.') , 1)
From DataCleaningProject..HousingData

ALTER TABLE DataCleaningProject..HousingData
Add OwnerSplitAddress Nvarchar(255);

Update DataCleaningProject..HousingData
SET OwnerSplitAddress = PARSENAME(REPLACE(OwnerAddress, ',', '.') , 3)

ALTER TABLE DataCleaningProject..HousingData
Add OwnerSplitCity Nvarchar(255);

Update DataCleaningProject..HousingData
SET OwnerSplitCity = PARSENAME(REPLACE(OwnerAddress, ',', '.') , 1)

ALTER TABLE DataCleaningProject..HousingData
Add OwnerSplitState Nvarchar(255);

Update DataCleaningProject..HousingData
SET OwnerSplitState = PARSENAME(REPLACE(OwnerAddress, ',', '.') , 1)

Select *
From DataCleaningProject..HousingData

--Y = Yes, N = No

Select Distinct(SoldAsVacant), COUNT(SoldAsVacant)
From DataCleaningProject..HousingData
Group by SoldAsVacant
order by 2

Select SoldAsVacant
, CASE When SoldAsVacant = 'Y' THEN 'Yes'
		When SoldAsVacant = 'N' THEN 'No'
		ELSE SoldAsVacant
		END
FROM DataCleaningProject..HousingData

-- Remove Duplicates

WITH RowNumCTE AS(
Select *,
	ROW_NUMBER() OVER(
	PARTITION BY ParcelID,
				 PropertyAddress,
				 SalePrice,
				 SaleDate,
				 LegalReference
				 ORDER BY 
					UniqueID
					) row_num
From DataCleaningProject..HousingData
)
Select * 
From RowNumCTE
Where row_num > 1
Order by PropertyAddress

-- Delete Unused Columns

Select *
From DataCleaningProject..HousingData

ALTER TABLE DataCleaningProject..HousingData
DROP COLUMN OwnerAddress, TaxDistrict, PropertyAddress,SaleDate