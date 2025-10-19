package edu.pjwstk.groups.usecase.createchatmessage;

import edu.pjwstk.groups.entity.ChatMessage;
import edu.pjwstk.groups.entity.Group;
import edu.pjwstk.groups.entity.GroupMember;

import java.util.UUID;

public interface CreateChatMessageMapper {
    ChatMessage toEntity(CreateChatMessageRequest request, Group group, UUID chatMessageId, GroupMember senderGroupMember);

    CreateChatMessageResponse toResponse(ChatMessage chatMessage);
}
