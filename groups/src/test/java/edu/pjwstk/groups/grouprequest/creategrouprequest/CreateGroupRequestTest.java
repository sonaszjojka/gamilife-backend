package edu.pjwstk.groups.grouprequest.creategrouprequest;

import edu.pjwstk.common.authApi.AuthApi;
import edu.pjwstk.common.authApi.dto.CurrentUserDto;
import edu.pjwstk.groups.entity.*;
import edu.pjwstk.groups.repository.GroupMemberRepository;
import edu.pjwstk.groups.repository.GroupRepository;
import edu.pjwstk.groups.repository.GroupRequestRepository;
import edu.pjwstk.groups.repository.GroupRequestStatusRepository;
import edu.pjwstk.groups.usecase.creategrouprequest.CreateGroupRequestMapper;
import edu.pjwstk.groups.usecase.creategrouprequest.CreateGroupRequestResponse;
import edu.pjwstk.groups.usecase.creategrouprequest.CreateGroupRequestUseCaseImpl;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.time.Instant;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
public class CreateGroupRequestTest {
    @Mock
    GroupRequestRepository groupRequestRepository;
    @Mock
    GroupRepository groupRepository;
    @Mock
    GroupMemberRepository groupMemberRepository;
    @Mock
    GroupRequestStatusRepository groupRequestStatusRepository;
    @Mock
    AuthApi authApi;
    @Mock
    CreateGroupRequestMapper createGroupRequestMapper;
    @InjectMocks
    CreateGroupRequestUseCaseImpl createGroupRequestUseCase;

    @Test
    void shouldCreateGroupRequestSuccessfully() {

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
                .groupType(GroupType.builder().groupTypeId(3).title("REQUEST_ONLY").build())
                .membersLimit(10)
                .groupMembers(List.of(administrator))
                .build();

        CurrentUserDto currentUser = new CurrentUserDto(userId, "test@example.com");

        GroupRequestStatus pendingStatus = GroupRequestStatus.builder()
                .groupRequestStatusId(1)
                .title("PENDING")
                .build();

        GroupRequest savedGroupRequest = GroupRequest.builder()
                .groupRequestId(UUID.randomUUID())
                .userId(userId)
                .groupRequested(group)
                .groupRequestStatus(pendingStatus)
                .createdAt(Instant.now())
                .build();

        CreateGroupRequestResponse expectedResponse = new CreateGroupRequestResponse(
                savedGroupRequest.getGroupRequestId(),
                userId,
                new CreateGroupRequestResponse.GroupDto(groupId),
                savedGroupRequest.getCreatedAt(),
                new CreateGroupRequestResponse.GroupRequestStatusDto(1, "PENDING")
        );


        when(authApi.getCurrentUser()).thenReturn(Optional.of(currentUser));
        when(groupRepository.findById(groupId)).thenReturn(Optional.of(group));
        when(groupRequestStatusRepository.findById(1)).thenReturn(Optional.of(pendingStatus));
        when(groupRequestRepository.save(any(GroupRequest.class))).thenReturn(savedGroupRequest);
        when(createGroupRequestMapper.toEntity(any(), any(), any(), any())).thenReturn(savedGroupRequest);
        when(createGroupRequestMapper.toResponse(savedGroupRequest)).thenReturn(expectedResponse);

        CreateGroupRequestResponse result = createGroupRequestUseCase.execute(groupId);


        assertThat(result).isNotNull();
        assertThat(result.groupRequestId()).isEqualTo(savedGroupRequest.getGroupRequestId());
        assertThat(result.userId()).isEqualTo(userId);
        assertThat(result.groupRequested().groupId()).isEqualTo(groupId);
        assertThat(result.groupRequestStatus().title()).isEqualTo("PENDING");

        verify(authApi).getCurrentUser();
        verify(groupRepository).findById(groupId);
        verify(groupRequestStatusRepository).findById(1);
        verify(groupRequestRepository).save(any(GroupRequest.class));
        verify(createGroupRequestMapper).toEntity(any(), eq(group), eq(pendingStatus), eq(userId));
        verify(createGroupRequestMapper).toResponse(savedGroupRequest);

    }


}
