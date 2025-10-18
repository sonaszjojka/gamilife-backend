package edu.pjwstk.groups.usecase.creategroupmember.creategroupmemberafteracceptation;

import edu.pjwstk.common.groupsApi.exception.GroupNotFoundException;
import edu.pjwstk.common.userApi.UserApi;
import edu.pjwstk.common.userApi.dto.BasicUserInfoApiDto;
import edu.pjwstk.common.userApi.exception.UserNotFoundException;
import edu.pjwstk.groups.entity.Group;
import edu.pjwstk.groups.entity.GroupMember;
import edu.pjwstk.groups.exception.GroupFullException;
import edu.pjwstk.groups.exception.UserAlreadyMemberOfGroupException;
import edu.pjwstk.groups.repository.GroupMemberRepository;
import edu.pjwstk.groups.repository.GroupRepository;
import edu.pjwstk.groups.usecase.creategroupmember.CreateGroupMemberMapper;
import edu.pjwstk.groups.usecase.creategroupmember.CreateGroupMemberRequest;
import edu.pjwstk.groups.usecase.creategroupmember.CreateGroupMemberResponse;
import jakarta.validation.Valid;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.UUID;

@Service
public class CreateGroupMemberAfterAcceptationUseCaseImpl implements CreateGroupMemberAfterAcceptationUseCase {

    private final GroupMemberRepository groupMemberRepository;
    private final GroupRepository groupRepository;
    private final UserApi userApi;
    private final CreateGroupMemberMapper createGroupMemberMapper;

    public CreateGroupMemberAfterAcceptationUseCaseImpl(GroupMemberRepository groupMemberRepository,
                                                        GroupRepository groupRepository,
                                                        UserApi userApi, CreateGroupMemberMapper createGroupMemberMapper) {
        this.groupMemberRepository = groupMemberRepository;
        this.groupRepository = groupRepository;
        this.userApi = userApi;
        this.createGroupMemberMapper = createGroupMemberMapper;
    }

    @Override
    @Transactional
    public CreateGroupMemberResponse execute(@Valid CreateGroupMemberAfterAcceptationRequest request) {
        Group group = groupRepository.findById(request.groupId())
                .orElseThrow(() -> new GroupNotFoundException("Group with id: " + request.groupId() + " not found!"));

        BasicUserInfoApiDto userInfoApiDto = userApi.getUserById(request.userId())
                .orElseThrow(() -> new UserNotFoundException("User with id: " + request.userId() + " not found!"));

        if (groupMemberRepository.existsByUserIdAndGroup(group, userInfoApiDto.userId())) {
            throw new UserAlreadyMemberOfGroupException("User with id: " + userInfoApiDto.userId()
                    + " is already member of group with id: " + request.groupId() + "!");
        }

        if (group.getGroupMembers().size() >= group.getMembersLimit()) {
            throw new GroupFullException("Group with id: " + request.groupId() + " is full!");
        }

        GroupMember groupMember = createGroupMemberMapper.toEntity(userInfoApiDto.userId(), group);
        GroupMember savedGroupMember = groupMemberRepository.save(groupMember);
        return createGroupMemberMapper.toResponse(savedGroupMember);
    }
}
