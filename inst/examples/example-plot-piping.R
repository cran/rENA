# Load test data
data(RS.data);

# Define codenames for the test
codenames <- c("Data", "Technical.Constraints", "Performance.Parameters",
  "Client.and.Consultant.Requests", "Design.Reasoning", "Collaboration");

# Standard ENA accumulation
accum <- rENA:::ena.accumulate.data.file(
  RS.data, units.by = c("UserName", "Condition"),
  conversations.by = c("ActivityNumber", "GroupName"),
  codes = codenames
);
# Create a standard ENA set 
newset <- ena.make.set(accum);

# Simple plot for a set of points, and their mean
newplot <- plot(newset) |> 
  add_points(Condition$FirstGame, mean = TRUE, colors = "blue") |> 
  add_network();

# Plot a single unit's point and it's line.weights
plot(newset) |> 
  add_points(UserName$`steven z`) |> 
  add_network();

# Trajectory accumulation and plotting
trajectory_accumulation <- rENA:::ena.accumulate.data.file(
  RS.data, units.by = c("UserName", "Condition"),
  conversations.by = c("ActivityNumber", "GroupName"),
  codes = codenames,
  model = "A"
);
trajectory_model <- ena.make.set(trajectory_accumulation);

newplot3 <- plot(trajectory_model) |> 
  add_trajectory(Condition$FirstGame);
