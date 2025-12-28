package pl.gamilife.group.usecase.editgroupmember;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record EditGroupMemberCommand(
        @NotNull
        UUID userId,

        @NotNull
        UUID groupId,

        @NotNull
        UUID groupMemberId,

        @NotNull
        Integer groupMoney
) implements Command {
}
