package pl.gamilife.group.usecase.creategroupmemberinopengroup;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record CreateGroupMemberInOpenGroupCommand(
        @NotNull
        UUID groupId,

        @NotNull
        UUID userId
) implements Command {
}
