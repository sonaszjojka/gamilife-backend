package pl.gamilife.group.usecase.findallgroupsbyuseridwhereuserismember;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record FindAllGroupsByUserIdWhereUserIsMemberCommand(
        @NotNull
        UUID userId,

        @NotNull
        Integer page,

        @NotNull
        Integer size,

        Integer groupType,

        String groupName
) implements Command {
}
