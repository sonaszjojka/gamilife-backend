package edu.pjwstk.groups.usecase.createchatmessage;

import lombok.Builder;

import java.io.Serializable;
import java.time.Instant;
import java.util.UUID;

/**
 * DTO for {@link edu.pjwstk.groups.model.ChatMessage}
 */
@Builder
public record CreateChatMessageResponse(UUID messageId, Boolean isImportant, Instant sendAt, GroupDto group,
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
    public record GroupMemberDto(UUID groupMemberId, UUID userId, Instant joinedAt) implements Serializable {
    }
}