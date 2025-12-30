package pl.gamilife.group.usecase.getgrouprequests;

import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Positive;
import jakarta.validation.constraints.PositiveOrZero;
import pl.gamilife.shared.kernel.architecture.Command;

import java.io.Serializable;
import java.util.UUID;

public record GetGroupRequestsCommand(
        @NotNull
        UUID groupId,
        Integer statusId,

        @NotNull
        @PositiveOrZero
        Integer page,

        @NotNull
        @Positive
        Integer size
) implements Command, Serializable {
}
