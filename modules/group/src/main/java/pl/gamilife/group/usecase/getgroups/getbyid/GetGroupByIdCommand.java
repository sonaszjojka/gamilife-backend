package pl.gamilife.group.usecase.getgroups.getbyid;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.io.Serializable;
import java.util.UUID;

public record GetGroupByIdCommand(
        @NotNull
        UUID userId,

        @NotNull
        UUID groupId,

        Boolean isForLoggedUser
) implements Command, Serializable {
}
