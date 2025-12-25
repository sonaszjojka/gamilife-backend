package pl.gamilife.gamification.application.usecase.getrequiredexperiencefornextlevel;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

public record GetRequiredExperienceForNextLevelCommand(@NotNull Integer currentLevel) implements Command {
}
