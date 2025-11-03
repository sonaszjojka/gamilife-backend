package edu.pjwstk.groups.groupmember.findgroupmemberbyid;

import edu.pjwstk.common.groupsApi.dto.GroupMemberDto;
import edu.pjwstk.groups.entity.Group;
import edu.pjwstk.groups.entity.GroupMember;
import edu.pjwstk.groups.entity.GroupType;
import edu.pjwstk.groups.repository.GroupMemberRepository;
import edu.pjwstk.groups.usecase.findgroupmemberbyid.FindGroupMemberByIdUseCaseImpl;
import edu.pjwstk.groups.usecase.findgroupmemberbyid.GroupMemberMapper;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
public class FindGroupMemberByIdTest {

    @Mock
    GroupMemberRepository groupMemberRepository;

    @Mock
    GroupMemberMapper groupMemberMapper;

    @InjectMocks
    private FindGroupMemberByIdUseCaseImpl findGroupMemberByIdUseCase;

    @Test
    void shouldFindGroupMemberByIdSuccessfully() {
        UUID groupMemberId = UUID.randomUUID();
        UUID groupId = UUID.randomUUID();
        UUID userId = UUID.randomUUID();
        UUID adminId = UUID.randomUUID();

        GroupMember administrator = GroupMember.builder()
                .groupMemberId(UUID.randomUUID())
                .userId(adminId)
                .build();

        Group group = Group.builder()
                .groupId(groupId)
                .adminId(adminId)
                .groupType(GroupType.builder().groupTypeId(1).title("Open").build())
                .membersLimit(10)
                .groupMembers(List.of(administrator))
                .build();

        GroupMember expectedMember = GroupMember.builder()
                .groupMemberId(groupMemberId)
                .userId(userId)
                .memberGroup(group)
                .joinedAt(Instant.now().minusSeconds(10 * 24 * 60 * 60))
                .groupMoney(500)
                .totalEarnedMoney(2000)
                .leftAt(null)
                .build();

        GroupMemberDto expectedResponse = GroupMemberDto.builder()
                .groupMemberId(groupMemberId)
                .userId(userId)
                .memberGroup(
                        new GroupMemberDto.GroupDto(expectedMember.getMemberGroup().getGroupId())
                )
                .joinedAt(expectedMember.getJoinedAt())
                .leftAt(expectedMember.getLeftAt())
                .groupMoney(expectedMember.getGroupMoney())
                .totalEarnedMoney(expectedMember.getTotalEarnedMoney())
                .build();

        List<GroupMember> groupMembers = new ArrayList<>();
        groupMembers.add(administrator);
        groupMembers.add(expectedMember);
        group.setGroupMembers(groupMembers);

        when(groupMemberRepository.findById(groupMemberId)).thenReturn(Optional.of(expectedMember));
        when(groupMemberMapper.toResponse(any(GroupMember.class))).thenReturn(expectedResponse);

       GroupMemberDto response = findGroupMemberByIdUseCase.execute(groupMemberId);
       assertThat(response).isNotNull();
       assertThat(response).isEqualTo(expectedResponse);

        verify(groupMemberRepository).findById(groupMemberId);
        verify(groupMemberMapper).toResponse(expectedMember);


    }
}
