package pl.gamilife.group.usecase.findgroupmemberbyuserid;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record FindGroupMemberByUserIdCommand(@NotNull UUID userId, @NotNull UUID groupId) implements Command {
}
