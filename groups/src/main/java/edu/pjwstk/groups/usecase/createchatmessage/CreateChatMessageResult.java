package edu.pjwstk.groups.usecase.createchatmessage;

import lombok.Builder;

import java.io.Serializable;
import java.time.Instant;
import java.util.UUID;

/**
 * DTO for {@link edu.pjwstk.groups.model.ChatMessage}
 */
@Builder
public record CreateChatMessageResult(UUID messageId, Boolean isImportant, Instant sendAt, GroupDto group,
                                      String content,
                                      GroupMemberDto senderGroupMember) implements Serializable {
    /**
     * DTO for {@link edu.pjwstk.groups.model.Group}
     */
    public record GroupDto(UUID groupId) implements Serializable {
    }

    /**
     * DTO for {@link edu.pjwstk.groups.model.GroupMember}
     */
    public record GroupMemberDto(UUID groupMemberId) implements Serializable {
    }
}