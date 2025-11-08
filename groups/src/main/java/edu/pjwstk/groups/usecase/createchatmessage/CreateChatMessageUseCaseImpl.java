package edu.pjwstk.groups.usecase.createchatmessage;

import edu.pjwstk.api.auth.AuthApi;
import edu.pjwstk.api.auth.dto.CurrentUserDto;
import edu.pjwstk.core.exception.common.domain.GroupMemberNotFoundException;
import edu.pjwstk.core.exception.common.domain.GroupNotFoundException;
import edu.pjwstk.groups.entity.ChatMessage;
import edu.pjwstk.groups.entity.Group;
import edu.pjwstk.groups.entity.GroupMember;
import edu.pjwstk.groups.exception.domain.UserLeftGroupException;
import edu.pjwstk.core.exception.common.domain.ResourceOwnerPrivilegesRequiredException;
import edu.pjwstk.groups.repository.ChatMessageJpaRepository;
import edu.pjwstk.groups.repository.GroupJpaRepository;
import edu.pjwstk.groups.repository.GroupMemberJpaRepository;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Objects;
import java.util.UUID;

@Service
public class CreateChatMessageUseCaseImpl implements CreateChatMessageUseCase {

    private final ChatMessageJpaRepository chatMessageRepository;
    private final GroupMemberJpaRepository groupMemberRepository;
    private final GroupJpaRepository groupRepository;
    private final CreateChatMessageMapper createChatMessageMapper;
    private final AuthApi authApi;

    public CreateChatMessageUseCaseImpl(ChatMessageJpaRepository chatMessageRepository,
                                        GroupMemberJpaRepository groupMemberRepository, GroupJpaRepository groupRepository,
                                        CreateChatMessageMapper createChatMessageMapper, AuthApi authApi) {
        this.chatMessageRepository = chatMessageRepository;
        this.groupMemberRepository = groupMemberRepository;
        this.groupRepository = groupRepository;
        this.createChatMessageMapper = createChatMessageMapper;
        this.authApi = authApi;
    }

    @Override
    @Transactional
    public CreateChatMessageResponse execute(CreateChatMessageRequest request, UUID groupId, UUID groupMemberId) {
        Group group = groupRepository.findById(groupId)
                .orElseThrow(
                        () -> new GroupNotFoundException("Group with id: " + groupId + " not found!")
                );

        GroupMember groupMember = groupMemberRepository.findById(groupMemberId)
                .orElseThrow(() -> new GroupMemberNotFoundException("Group member with id: " + groupMemberId + " not found!"));

        CurrentUserDto currentUserDto = authApi.getCurrentUser();

        if (groupMember.getLeftAt() != null) {
            throw new UserLeftGroupException("Group member with id: " + groupMemberId + " left group with id: "
                    + groupMember.getMemberGroup().getGroupId() + " and is no longer member of it!");
        }

        if (!Objects.equals(currentUserDto.userId(), groupMember.getUserId())) {
            throw new ResourceOwnerPrivilegesRequiredException("User with id: " + groupMember.getUserId()
                    + " is not group member with id: " + groupMemberId);
        }

        ChatMessage chatMessage = createChatMessageMapper.toEntity(request, group, UUID.randomUUID(), groupMember);
        ChatMessage savedChatMessage = chatMessageRepository.save(chatMessage);

        return createChatMessageMapper.toResponse(savedChatMessage);
    }
}
