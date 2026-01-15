package pl.gamilife.group.model;

import org.instancio.Instancio;
import org.junit.jupiter.api.Test;
import pl.gamilife.group.enums.GroupRequestStatusEnum;
import pl.gamilife.shared.kernel.exception.domain.DomainValidationException;

import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.catchThrowable;
import static org.instancio.Select.field;

class GroupRequestTest {

    @Test
    void shouldCreateGroupRequest_whenValidDataIsProvided() {
        // given
        Group group = Instancio.create(Group.class);
        UUID userId = UUID.randomUUID();
        GroupRequestStatus status = Instancio.create(GroupRequestStatus.class);

        // when
        GroupRequest request = GroupRequest.create(group, userId, status);

        // then
        assertThat(request).isNotNull();
        assertThat(request.getGroup()).isEqualTo(group);
        assertThat(request.getGroupId()).isEqualTo(group.getId());
        assertThat(request.getUserId()).isEqualTo(userId);
        assertThat(request.getStatus()).isEqualTo(status);
        assertThat(request.getStatusId()).isEqualTo(status.getId());
    }

    @Test
    void shouldReturnTrue_whenRequestBelongsToUser() {
        // given
        UUID userId = UUID.randomUUID();
        GroupRequest request = Instancio.of(GroupRequest.class)
                .set(field(GroupRequest::getUserId), userId)
                .create();

        // when
        boolean result = request.belongsToUser(userId);

        // then
        assertThat(result).isTrue();
    }

    @Test
    void shouldReturnFalse_whenRequestDoesNotBelongToUser() {
        // given
        UUID userId = UUID.randomUUID();
        GroupRequest request = Instancio.of(GroupRequest.class)
                .set(field(GroupRequest::getUserId), UUID.randomUUID())
                .create();

        // when
        boolean result = request.belongsToUser(userId);

        // then
        assertThat(result).isFalse();
    }

    @Test
    void shouldReturnTrue_whenRequestHasStatus() {
        // given
        GroupRequestStatusEnum expectedEnum = GroupRequestStatusEnum.values()[0];

        GroupRequestStatus status = Instancio.of(GroupRequestStatus.class)
                .set(field(GroupRequestStatus::getId), expectedEnum.getId())
                .create();

        GroupRequest request = Instancio.of(GroupRequest.class)
                .set(field(GroupRequest::getStatus), status)
                .create();

        // when
        boolean result = request.hasStatus(expectedEnum);

        // then
        assertThat(result).isTrue();
    }

    @Test
    void shouldChangeStatus_whenStatusIsValid() {
        // given
        GroupRequest request = Instancio.create(GroupRequest.class);
        GroupRequestStatus newStatus = Instancio.create(GroupRequestStatus.class);

        // when
        request.changeStatus(newStatus);

        // then
        assertThat(request.getStatus()).isEqualTo(newStatus);
        assertThat(request.getStatusId()).isEqualTo(newStatus.getId());
    }

    @Test
    void shouldThrowException_whenChangingStatusToNull() {
        // given
        GroupRequest request = Instancio.create(GroupRequest.class);

        // when
        Throwable thrown = catchThrowable(() -> request.changeStatus(null));

        // then
        assertThat(thrown)
                .isInstanceOf(DomainValidationException.class)
                .hasMessage("Group request status cannot be null");
    }

    @Test
    void shouldThrowException_whenGroupIsNull() {
        // given
        UUID userId = UUID.randomUUID();
        GroupRequestStatus status = Instancio.create(GroupRequestStatus.class);

        // when
        Throwable thrown = catchThrowable(() -> GroupRequest.create(null, userId, status));

        // then
        assertThat(thrown)
                .isInstanceOf(DomainValidationException.class)
                .hasMessage("Group cannot be null");
    }

    @Test
    void shouldThrowException_whenUserIdIsNull() {
        // given
        Group group = Instancio.create(Group.class);
        GroupRequestStatus status = Instancio.create(GroupRequestStatus.class);

        // when
        Throwable thrown = catchThrowable(() -> GroupRequest.create(group, null, status));

        // then
        assertThat(thrown)
                .isInstanceOf(DomainValidationException.class)
                .hasMessage("User id cannot be null");
    }

    @Test
    void shouldThrowException_whenInitialStatusIsNull() {
        // given
        Group group = Instancio.create(Group.class);
        UUID userId = UUID.randomUUID();

        // when
        Throwable thrown = catchThrowable(() -> GroupRequest.create(group, userId, null));

        // then
        assertThat(thrown)
                .isInstanceOf(DomainValidationException.class)
                .hasMessage("Group request status cannot be null");
    }
}