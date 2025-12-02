package edu.pjwstk.groups.usecase.createchatmessage;

import edu.pjwstk.api.auth.AuthApi;
import edu.pjwstk.api.auth.dto.CurrentUserDto;
import pl.gamilife.infrastructure.core.exception.common.domain.GroupMemberNotFoundException;
import pl.gamilife.infrastructure.core.exception.common.domain.GroupNotFoundException;
import pl.gamilife.infrastructure.core.exception.common.domain.ResourceOwnerPrivilegesRequiredException;
import edu.pjwstk.groups.exception.domain.UserLeftGroupException;
import edu.pjwstk.groups.model.ChatMessage;
import edu.pjwstk.groups.model.Group;
import edu.pjwstk.groups.model.GroupMember;
import edu.pjwstk.groups.repository.ChatMessageJpaRepository;
import edu.pjwstk.groups.repository.GroupJpaRepository;
import edu.pjwstk.groups.repository.GroupMemberJpaRepository;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.UUID;

@Service
@Transactional
@AllArgsConstructor
public class CreateChatMessageUseCaseImpl implements CreateChatMessageUseCase {

    private final ChatMessageJpaRepository chatMessageRepository;
    private final GroupMemberJpaRepository groupMemberRepository;
    private final GroupJpaRepository groupRepository;
    private final AuthApi authApi;

    @Override
    public CreateChatMessageResult execute(CreateChatMessageCommand cmd) {
        Group group = getGroup(cmd.groupId());
        GroupMember groupMember = getGroupMember(cmd.groupId(), cmd.groupMemberId());
        CurrentUserDto currentUserDto = authApi.getCurrentUser();

        if (!groupMember.isActive()) {
            throw new UserLeftGroupException("Group member with id: " + cmd.groupMemberId() + " left group with id: "
                    + group.getGroupId() + " and is no longer member of it!");
        }

        if (groupMember.isUser(currentUserDto.userId())) {
            throw new ResourceOwnerPrivilegesRequiredException("User with id: " + currentUserDto.userId()
                    + " is not a group member with id: " + cmd.groupMemberId());
        }

        ChatMessage chatMessage = ChatMessage.builder()
                .messageId(UUID.randomUUID())
                .isImportant(cmd.isImportant())
                .group(group)
                .content(cmd.content())
                .groupMember(groupMember)
                .build();
        ChatMessage savedChatMessage = chatMessageRepository.save(chatMessage);

        return buildCreateChatMessageResult(savedChatMessage);
    }

    private Group getGroup(UUID groupId) {
        return groupRepository.findById(groupId)
                .orElseThrow(
                        () -> new GroupNotFoundException("Group with id: " + groupId + " not found!")
                );
    }

    private GroupMember getGroupMember(UUID groupId, UUID groupMemberId) {
        return groupMemberRepository.findByGroupMemberIdAndGroupId(groupMemberId, groupId)
                .orElseThrow(
                        () -> new GroupMemberNotFoundException("Group member with id: " + groupMemberId + " not found!")
                );
    }

    private CreateChatMessageResult buildCreateChatMessageResult(ChatMessage chatMessage) {
        return CreateChatMessageResult.builder()
                .messageId(chatMessage.getMessageId())
                .isImportant(chatMessage.getIsImportant())
                .sendAt(chatMessage.getSentAt())
                .content(chatMessage.getContent())
                .group(new CreateChatMessageResult.GroupDto(chatMessage.getGroupId()))
                .senderGroupMember(new CreateChatMessageResult.GroupMemberDto(
                        chatMessage.getGroupMemberId()
                ))
                .build();
    }
}
