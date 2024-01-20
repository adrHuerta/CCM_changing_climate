import os
import xarray as xr
import pandas as pd

tmax_file = os.path.join(os.path.dirname(os.path.dirname(os.getcwd())), "datasets", "grid", "others", "PISCOt_v12", "tmax_daily_1981_2020_010.nc")
tmin_file = os.path.join(os.path.dirname(os.path.dirname(os.getcwd())), "datasets", "grid", "others", "PISCOt_v12", "tmin_daily_1981_2020_010.nc")

tmax_file = xr.open_dataset(tmax_file)
tmin_file = xr.open_dataset(tmin_file)

tmax_file = tmax_file.resample(time="YE").mean().mean(dim = ["latitude","longitude"]).to_dataframe()
tmin_file = tmin_file.resample(time="YE").mean().mean(dim = ["latitude","longitude"]).to_dataframe()

df = pd.DataFrame({"years":tmin_file.index.year, "values":(tmax_file.reset_index()["tmax"] + tmin_file.reset_index()["tmin"])/2})
df.to_csv(os.path.join(os.getcwd(), "data", "PISCOt_Tmean_value.csv"))
