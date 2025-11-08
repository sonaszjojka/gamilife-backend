package edu.pjwstk.groups.usecase.createchatmessage;

import edu.pjwstk.groups.model.ChatMessage;
import edu.pjwstk.groups.model.Group;
import edu.pjwstk.groups.model.GroupMember;
import org.springframework.stereotype.Component;

import java.util.UUID;

@Component
public class CreateChatMessageMapperImpl implements CreateChatMessageMapper {
    @Override
    public ChatMessage toEntity(CreateChatMessageRequest request, Group group,
                                UUID chatMessageId, GroupMember senderGroupMember) {
        return ChatMessage.builder()
                .messageId(chatMessageId)
                .isImportant(request.isImportant())
                .group(group)
                .content(request.content())
                .senderGroupMember(senderGroupMember)
                .build();
    }

    @Override
    public CreateChatMessageResponse toResponse(ChatMessage chatMessage) {
        return CreateChatMessageResponse.builder()
                .messageId(chatMessage.getMessageId())
                .isImportant(chatMessage.getIsImportant())
                .sendAt(chatMessage.getSendAt())
                .content(chatMessage.getContent())
                .group(new CreateChatMessageResponse.GroupDto(chatMessage.getGroup().getGroupId()))
                .senderGroupMember(new CreateChatMessageResponse.GroupMemberDto(
                        chatMessage.getSenderGroupMember().getGroupMemberId(),
                        chatMessage.getSenderGroupMember().getUserId(),
                        chatMessage.getSenderGroupMember().getJoinedAt()
                ))
                .build();
    }

}
