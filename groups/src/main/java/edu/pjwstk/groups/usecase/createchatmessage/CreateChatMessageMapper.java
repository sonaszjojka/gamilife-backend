package edu.pjwstk.groups.usecase.createchatmessage;

import edu.pjwstk.groups.model.ChatMessage;
import edu.pjwstk.groups.model.Group;
import edu.pjwstk.groups.model.GroupMember;

import java.util.UUID;

public interface CreateChatMessageMapper {
    ChatMessage toEntity(CreateChatMessageRequest request, Group group, UUID chatMessageId, GroupMember senderGroupMember);

    CreateChatMessageResponse toResponse(ChatMessage chatMessage);
}
