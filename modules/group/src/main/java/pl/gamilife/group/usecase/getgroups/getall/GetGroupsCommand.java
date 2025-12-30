package pl.gamilife.group.usecase.getgroups.getall;

import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Positive;
import jakarta.validation.constraints.PositiveOrZero;
import pl.gamilife.shared.kernel.architecture.Command;

import java.io.Serializable;

public record GetGroupsCommand(
        String joinCode,
        Integer type,
        String name,

        @NotNull
        @PositiveOrZero
        Integer page,

        @NotNull
        @Positive
        Integer size
) implements Command, Serializable {
}
