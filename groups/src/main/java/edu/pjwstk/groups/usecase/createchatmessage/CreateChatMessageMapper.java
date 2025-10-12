package edu.pjwstk.groups.usecase.createchatmessage;

import edu.pjwstk.groups.domain.ChatMessage;
import edu.pjwstk.groups.domain.Group;
import edu.pjwstk.groups.domain.GroupMember;

import java.util.UUID;

public interface CreateChatMessageMapper {
    ChatMessage toEntity(CreateChatMessageRequest request, Group group, UUID chatMessageId, GroupMember senderGroupMember);

    CreateChatMessageResponse toResponse(ChatMessage chatMessage);
}
