package edu.pjwstk.groups.usecase.editgroupmember;

import edu.pjwstk.core.exception.common.domain.GroupMemberNotFoundException;
import edu.pjwstk.groups.entity.GroupMember;
import edu.pjwstk.groups.exception.domain.UserLeftGroupException;
import edu.pjwstk.groups.repository.GroupMemberJpaRepository;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
public class EditGroupMemberUseCaseImpl implements EditGroupMemberUseCase {

    private final GroupMemberJpaRepository groupMemberRepository;
    private final EditGroupMemberMapper editGroupMemberMapper;

    public EditGroupMemberUseCaseImpl(GroupMemberJpaRepository groupMemberRepository, EditGroupMemberMapper editGroupMemberMapper) {
        this.groupMemberRepository = groupMemberRepository;
        this.editGroupMemberMapper = editGroupMemberMapper;
    }

    @Override
    public EditGroupMemberResponse execute(UUID groupMemberId, EditGroupMemberRequest request) {
        GroupMember groupMember = groupMemberRepository.findById(groupMemberId)
                .orElseThrow(() -> new GroupMemberNotFoundException("Group member with id: "
                        + groupMemberId + " not found!"));

        if (groupMember.getLeftAt() != null) {
            throw new UserLeftGroupException("Group member with id: " + groupMemberId + " left group with id: "
                    + groupMember.getMemberGroup().getGroupId() + " and is no longer member of it!");
        }

        groupMember.setGroupMoney(request.groupMoney());
        groupMember.setTotalEarnedMoney(request.totalEarnedMoney());

        return editGroupMemberMapper.toResponse(groupMemberRepository.save(groupMember));
    }
}
