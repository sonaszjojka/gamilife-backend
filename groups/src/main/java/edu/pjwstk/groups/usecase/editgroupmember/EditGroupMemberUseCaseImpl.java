package edu.pjwstk.groups.usecase.editgroupmember;

import edu.pjwstk.common.groupsApi.exception.GroupMemberNotFoundException;
import edu.pjwstk.groups.api.controller.GroupMemberController;
import edu.pjwstk.groups.entity.GroupMember;
import edu.pjwstk.groups.repository.GroupMemberRepository;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
public class EditGroupMemberUseCaseImpl implements EditGroupMemberUseCase {

    private final GroupMemberRepository groupMemberRepository;
    private final EditGroupMemberMapper editGroupMemberMapper;

    public EditGroupMemberUseCaseImpl(GroupMemberRepository groupMemberRepository, EditGroupMemberMapper editGroupMemberMapper) {
        this.groupMemberRepository = groupMemberRepository;
        this.editGroupMemberMapper = editGroupMemberMapper;
    }

    @Override
    public EditGroupMemberResponse execute(UUID groupMemberId, EditGroupMemberRequest request) {
        GroupMember groupMember = groupMemberRepository.findById(groupMemberId)
                .orElseThrow(() -> new GroupMemberNotFoundException("Group member with id: "
                        + groupMemberId + " not found!"));

        groupMember.setGroupMoney(request.groupMoney());
        groupMember.setTotalEarnedMoney(request.totalEarnedMoney());

        return editGroupMemberMapper.toResponse(groupMemberRepository.save(groupMember));
    }
}
