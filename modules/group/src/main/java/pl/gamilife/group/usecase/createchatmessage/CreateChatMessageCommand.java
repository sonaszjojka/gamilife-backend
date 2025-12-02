package pl.gamilife.group.usecase.createchatmessage;

import pl.gamilife.infrastructure.core.architecture.Command;
import jakarta.validation.ValidationException;

import java.util.UUID;

public record CreateChatMessageCommand(
        UUID groupId,
        UUID groupMemberId,
        String content,
        Boolean isImportant
) implements Command {

    @Override
    public void validate() {
        if (groupId == null || groupMemberId == null) {
            throw new ValidationException("GroupId and GroupMemberId cannot be null!");
        }

        if (content == null || content.isBlank()) {
            throw new ValidationException("Content cannot be blank!");
        }
    }
}
