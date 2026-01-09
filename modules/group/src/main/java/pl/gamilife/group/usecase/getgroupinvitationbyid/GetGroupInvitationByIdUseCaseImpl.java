package pl.gamilife.group.usecase.getgroupinvitationbyid;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import pl.gamilife.group.exception.domain.GroupInvitationNotFoundException;
import pl.gamilife.group.repository.GroupInvitationJpaRepository;
import pl.gamilife.shared.kernel.exception.domain.ResourceOwnerPrivilegesRequiredException;

@Service
@AllArgsConstructor
public class GetGroupInvitationByIdUseCaseImpl implements GetGroupInvitationByIdUseCase {

    private final GroupInvitationJpaRepository groupInvitationRepository;

    @Override
    public GetGroupInvitationByIdResult execute(GetGroupInvitationByIdCommand cmd) {
        var groupInvitation = groupInvitationRepository.findWithGroupByIdAndGroupId(cmd.groupInvitationId(), cmd.groupId())
                .orElseThrow(() -> new GroupInvitationNotFoundException(String.format(
                        "Group invitation with id %s not found!",
                        cmd.groupInvitationId()
                )));

        var group = groupInvitation.getGroup();
        if (!groupInvitation.doesBelongToUser(cmd.userId()) && !group.isUserAdmin(cmd.userId())) {
            throw new ResourceOwnerPrivilegesRequiredException("You do not have permission to access this invitation");
        }

        return new GetGroupInvitationByIdResult(
                groupInvitation.getId(),
                group.getId(),
                group.getName(),
                groupInvitation.getUserId(),
                switch (groupInvitation.getStatus().toEnum()) {
                    case SENT  -> GetGroupInvitationByIdResult.InvitationStatus.SENT;
                    case ACCEPTED -> GetGroupInvitationByIdResult.InvitationStatus.ACCEPTED;
                    case DECLINED -> GetGroupInvitationByIdResult.InvitationStatus.DECLINED;
                    case REVOKED -> GetGroupInvitationByIdResult.InvitationStatus.REVOKED;
                },
                groupInvitation.getExpiresAt().toEpochMilli()
        );
    }
}
