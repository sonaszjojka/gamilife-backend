package edu.pjwstk.groups.usecase.createchatmessage;

import edu.pjwstk.common.authApi.AuthApi;
import edu.pjwstk.common.authApi.dto.CurrentUserDto;
import edu.pjwstk.common.groupsApi.exception.GroupMemberNotFoundException;
import edu.pjwstk.common.groupsApi.exception.GroupNotFoundException;
import edu.pjwstk.groups.entity.ChatMessage;
import edu.pjwstk.groups.entity.Group;
import edu.pjwstk.groups.entity.GroupMember;
import edu.pjwstk.groups.exception.UserLeftGroupException;
import edu.pjwstk.groups.exception.UserNotOwnerAccessDeniedException;
import edu.pjwstk.groups.repository.ChatMessageRepository;
import edu.pjwstk.groups.repository.GroupMemberRepository;
import edu.pjwstk.groups.repository.GroupRepository;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Objects;
import java.util.UUID;

@Service
public class CreateChatMessageUseCaseImpl implements CreateChatMessageUseCase {

    private final ChatMessageRepository chatMessageRepository;
    private final GroupMemberRepository groupMemberRepository;
    private final GroupRepository groupRepository;
    private final CreateChatMessageMapper createChatMessageMapper;
    private final AuthApi authApi;

    public CreateChatMessageUseCaseImpl(ChatMessageRepository chatMessageRepository,
                                        GroupMemberRepository groupMemberRepository, GroupRepository groupRepository,
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

        CurrentUserDto currentUserDto = authApi.getCurrentUser()
                .orElseThrow();

        if (groupMember.getLeftAt() != null) {
            throw new UserLeftGroupException("Group member with id: " + groupMemberId + " left group with id: "
                    + groupMember.getMemberGroup().getGroupId() + " and is no longer member of it!");
        }

        if (!Objects.equals(currentUserDto.userId(), groupMember.getUserId())) {
            throw new UserNotOwnerAccessDeniedException("User with id: " + groupMember.getUserId()
                    + " is not group member with id: " + groupMemberId);
        }

        ChatMessage chatMessage = createChatMessageMapper.toEntity(request, group, UUID.randomUUID(), groupMember);
        ChatMessage savedChatMessage = chatMessageRepository.save(chatMessage);

        return createChatMessageMapper.toResponse(savedChatMessage);
    }
}
