package pl.gamilife.group.usecase.createchatmessage;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record CreateChatMessageCommand(
        @NotNull
        UUID groupId,

        @NotNull
        UUID groupMemberId,

        @NotBlank
        String content,
        Boolean isImportant
) implements Command {
}
