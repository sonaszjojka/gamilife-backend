package pl.gamilife.group.usecase.getchatmessages;


import java.io.Serializable;
import java.time.Instant;
import java.util.Collection;
import java.util.UUID;

public record GetChatMessagesResult(
        int totalElements,
        int totalPages,
        int currentPage,
        int pageSize,
        Collection<ChatMessageDto> content
) implements Serializable {

    public record ChatMessageDto(
            UUID messageId,
            String content,
            Instant sentAt,
            Boolean isImportant,
            UUID groupId,
            UUID groupMemberId,
            String senderUsername
    ) implements Serializable {
    }
}
