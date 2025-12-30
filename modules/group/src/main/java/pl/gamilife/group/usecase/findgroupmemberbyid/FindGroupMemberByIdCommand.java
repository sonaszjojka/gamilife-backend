package pl.gamilife.group.usecase.findgroupmemberbyid;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record FindGroupMemberByIdCommand(@NotNull UUID groupId, @NotNull UUID groupMemberId) implements Command {
}
