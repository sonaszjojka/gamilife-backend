package pl.gamilife.group.usecase.createchatmessage;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.api.auth.AuthApi;
import pl.gamilife.api.auth.dto.CurrentUserDto;
import pl.gamilife.group.exception.domain.UserLeftGroupException;
import pl.gamilife.group.model.ChatMessage;
import pl.gamilife.group.model.Group;
import pl.gamilife.group.model.GroupMember;
import pl.gamilife.group.repository.ChatMessageJpaRepository;
import pl.gamilife.group.repository.GroupJpaRepository;
import pl.gamilife.group.repository.GroupMemberJpaRepository;
import pl.gamilife.shared.kernel.exception.domain.GroupMemberNotFoundException;
import pl.gamilife.shared.kernel.exception.domain.GroupNotFoundException;
import pl.gamilife.shared.kernel.exception.domain.ResourceOwnerPrivilegesRequiredException;

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

        if (!groupMember.isUser(currentUserDto.userId())) {
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
