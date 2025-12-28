package pl.gamilife.group.usecase.createchatmessage;

import lombok.Builder;
import pl.gamilife.group.model.ChatMessage;
import pl.gamilife.group.model.Group;
import pl.gamilife.group.model.GroupMember;

import java.io.Serializable;
import java.time.Instant;
import java.util.UUID;

/**
 * DTO for {@link ChatMessage}
 */
@Builder
public record CreateChatMessageResult(UUID messageId, Boolean isImportant, Instant sendAt, GroupDto group,
                                      String content,
                                      GroupMemberDto senderGroupMember) implements Serializable {
    /**
     * DTO for {@link Group}
     */
    public record GroupDto(UUID groupId) implements Serializable {
    }

    /**
     * DTO for {@link GroupMember}
     */
    public record GroupMemberDto(UUID groupMemberId) implements Serializable {
    }
}