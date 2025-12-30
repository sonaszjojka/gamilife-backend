package pl.gamilife.user.usecase.levelupuser;

import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Positive;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record LevelUpUserCommand(@NotNull UUID userId, @Positive @NotNull int level) implements Command {
}
