package edu.pjwstk.groups.usecase.createchatmessage;

import edu.pjwstk.common.groupsApi.exception.GroupMemberNotFoundException;
import edu.pjwstk.common.groupsApi.exception.GroupNotFoundException;
import edu.pjwstk.groups.domain.ChatMessage;
import edu.pjwstk.groups.domain.Group;
import edu.pjwstk.groups.domain.GroupMember;
import edu.pjwstk.groups.repository.ChatMessageRepository;
import edu.pjwstk.groups.repository.GroupMemberRepository;
import edu.pjwstk.groups.repository.GroupRepository;
import org.springframework.stereotype.Component;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
public class CreateChatMessageUseCaseImpl implements CreateChatMessageUseCase {

    private final ChatMessageRepository chatMessageRepository;
    private final GroupMemberRepository groupMemberRepository;
    private final GroupRepository groupRepository;
    private final CreateChatMessageMapper createChatMessageMapper;

    public CreateChatMessageUseCaseImpl(ChatMessageRepository chatMessageRepository,
                                        GroupMemberRepository groupMemberRepository, GroupRepository groupRepository,
                                        CreateChatMessageMapper createChatMessageMapper) {
        this.chatMessageRepository = chatMessageRepository;
        this.groupMemberRepository = groupMemberRepository;
        this.groupRepository = groupRepository;
        this.createChatMessageMapper = createChatMessageMapper;
    }

    @Override
    public CreateChatMessageResponse execute(CreateChatMessageRequest request, UUID groupId, Integer groupMemberId) {
        Group group = groupRepository.findById(groupId)
                .orElseThrow(
                        () -> new GroupNotFoundException("Group with id: " + groupId + " not found!")
                );

        GroupMember groupMember = groupMemberRepository.findById(groupMemberId)
                .orElseThrow(() -> new GroupMemberNotFoundException("Group member with id: " + groupMemberId + " not found!"));

        ChatMessage chatMessage = createChatMessageMapper.toEntity(request, group, UUID.randomUUID(), groupMember);
        ChatMessage savedChatMessage = chatMessageRepository.save(chatMessage);

        return createChatMessageMapper.toResponse(savedChatMessage);
    }
}
