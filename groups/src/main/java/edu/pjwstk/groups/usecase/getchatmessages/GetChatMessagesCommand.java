package edu.pjwstk.groups.usecase.getchatmessages;

import edu.pjwstk.core.Command;

import java.io.Serializable;
import java.util.UUID;

public record GetChatMessagesCommand(
        UUID groupId,
        Boolean isImportant,
        Integer page,
        Integer size
) implements Command, Serializable {
    @Override
    public void validate() {
        if (groupId == null) {
            throw new IllegalArgumentException("Group ID cannot be null");
        }
    }
}
