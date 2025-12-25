package pl.gamilife.group.usecase.getchatmessages;

import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Positive;
import jakarta.validation.constraints.PositiveOrZero;
import pl.gamilife.shared.kernel.architecture.Command;

import java.io.Serializable;
import java.util.UUID;

public record GetChatMessagesCommand(
        @NotNull
        UUID groupId,

        Boolean isImportant,

        @PositiveOrZero
        Integer page,

        @Positive
        Integer size
) implements Command, Serializable {
}
