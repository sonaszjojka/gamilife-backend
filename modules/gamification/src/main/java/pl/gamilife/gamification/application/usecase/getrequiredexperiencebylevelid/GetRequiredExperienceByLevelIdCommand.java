package pl.gamilife.gamification.application.usecase.getrequiredexperiencebylevelid;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

public record GetRequiredExperienceByLevelIdCommand(@NotNull Integer levelId) implements Command {
}
