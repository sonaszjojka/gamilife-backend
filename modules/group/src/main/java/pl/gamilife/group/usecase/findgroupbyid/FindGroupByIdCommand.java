package pl.gamilife.group.usecase.findgroupbyid;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record FindGroupByIdCommand(@NotNull UUID groupId) implements Command {
}
