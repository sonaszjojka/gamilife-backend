package edu.pjwstk.groups.usecase.createchatmessage;

import edu.pjwstk.api.auth.AuthApi;
import edu.pjwstk.api.auth.dto.CurrentUserDto;
import edu.pjwstk.core.exception.common.domain.GroupMemberNotFoundException;
import edu.pjwstk.core.exception.common.domain.GroupNotFoundException;
import edu.pjwstk.core.exception.common.domain.ResourceOwnerPrivilegesRequiredException;
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

import java.util.Objects;
import java.util.UUID;

@Service
@AllArgsConstructor
public class CreateChatMessageUseCaseImpl implements CreateChatMessageUseCase {

    private final ChatMessageJpaRepository chatMessageRepository;
    private final GroupMemberJpaRepository groupMemberRepository;
    private final GroupJpaRepository groupRepository;
    private final AuthApi authApi;

    @Override
    @Transactional
    public CreateChatMessageResult executeInternal(CreateChatMessageCommand cmd) {
        Group group = groupRepository.findById(cmd.groupId())
                .orElseThrow(
                        () -> new GroupNotFoundException("Group with id: " + cmd.groupId() + " not found!")
                );

        GroupMember groupMember = groupMemberRepository.findById(cmd.groupMemberId())
                .orElseThrow(() -> new GroupMemberNotFoundException("Group member with id: " + cmd.groupMemberId() + " not found!"));

        CurrentUserDto currentUserDto = authApi.getCurrentUser();

        if (groupMember.getLeftAt() != null) {
            throw new UserLeftGroupException("Group member with id: " + cmd.groupMemberId() + " left group with id: "
                    + groupMember.getMemberGroup().getGroupId() + " and is no longer member of it!");
        }

        if (!Objects.equals(currentUserDto.userId(), groupMember.getUserId())) {
            throw new ResourceOwnerPrivilegesRequiredException("User with id: " + groupMember.getUserId()
                    + " is not group member with id: " + cmd.groupMemberId());
        }

        ChatMessage chatMessage = ChatMessage.builder()
                .messageId(UUID.randomUUID())
                .isImportant(cmd.isImportant())
                .group(group)
                .content(cmd.content())
                .senderGroupMember(groupMember)
                .build();
        ChatMessage savedChatMessage = chatMessageRepository.save(chatMessage);

        return CreateChatMessageResult.builder()
                .messageId(savedChatMessage.getMessageId())
                .isImportant(savedChatMessage.getIsImportant())
                .sendAt(savedChatMessage.getSendAt())
                .content(savedChatMessage.getContent())
                .group(new CreateChatMessageResult.GroupDto(savedChatMessage.getGroup().getGroupId()))
                .senderGroupMember(new CreateChatMessageResult.GroupMemberDto(
                        savedChatMessage.getSenderGroupMember().getGroupMemberId(),
                        savedChatMessage.getSenderGroupMember().getUserId(),
                        savedChatMessage.getSenderGroupMember().getJoinedAt()
                ))
                .build();
    }
}
