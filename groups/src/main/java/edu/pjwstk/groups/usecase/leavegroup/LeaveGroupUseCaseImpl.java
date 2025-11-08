package edu.pjwstk.groups.usecase.leavegroup;

import edu.pjwstk.core.exception.common.domain.GroupMemberNotFoundException;
import edu.pjwstk.core.exception.common.domain.GroupNotFoundException;
import edu.pjwstk.groups.entity.Group;
import edu.pjwstk.groups.entity.GroupMember;
import edu.pjwstk.groups.exception.domain.AdminCannotLeaveGroupException;
import edu.pjwstk.groups.repository.GroupJpaRepository;
import edu.pjwstk.groups.repository.GroupMemberJpaRepository;
import org.springframework.stereotype.Service;

import java.time.Instant;
import java.util.Objects;
import java.util.UUID;

@Service
public class LeaveGroupUseCaseImpl implements LeaveGroupUseCase {

    private final GroupMemberJpaRepository groupMemberRepository;
    private final GroupJpaRepository groupRepository;
    private final LeaveGroupMapper leaveGroupMapper;

    public LeaveGroupUseCaseImpl(GroupMemberJpaRepository groupMemberRepository, GroupJpaRepository groupRepository, LeaveGroupMapper leaveGroupMapper) {
        this.groupMemberRepository = groupMemberRepository;
        this.groupRepository = groupRepository;
        this.leaveGroupMapper = leaveGroupMapper;
    }

    @Override
    public LeaveGroupResponse execute(UUID groupMemberId, UUID groupId) {
        GroupMember groupMember = groupMemberRepository.findById(groupMemberId)
                .orElseThrow(() -> new GroupMemberNotFoundException("Group member with id: "
                        + groupMemberId + " not found!"));

        Group group = groupRepository.findById(groupId)
                .orElseThrow(() -> new GroupNotFoundException("Group with id: " + groupId + " not found!"));

        if (Objects.equals(group.getAdminId(), groupMemberId) && group.getGroupMembers().size() > 1) {
            throw new AdminCannotLeaveGroupException("Administrator cannot leave group! " +
                    "Change group administrator before leaving.");
        }

        if (Objects.equals(group.getAdminId(), groupMemberId) && group.getGroupMembers().size() == 1) {
            throw new AdminCannotLeaveGroupException("Administrator cannot leave group! " +
                    "Delete group instead of leaving.");
        }

        groupMember.setLeftAt(Instant.now());
        GroupMember savedGroupMember = groupMemberRepository.save(groupMember);

        return leaveGroupMapper.toResponse(savedGroupMember);
    }
}
