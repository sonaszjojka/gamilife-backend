package pl.gamilife.group.usecase.getgrouptimezone;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record GetGroupTimezoneCommand(@NotNull UUID groupId) implements Command {
}
