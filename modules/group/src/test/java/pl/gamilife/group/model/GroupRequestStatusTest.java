package pl.gamilife.group.model;

import org.instancio.Instancio;
import org.junit.jupiter.api.Test;
import pl.gamilife.group.enums.GroupRequestStatusEnum;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.catchThrowable;
import static org.instancio.Select.field;

class GroupRequestStatusTest {

    @Test
    void shouldReturnCorrectEnum_whenIdIsValid() {
        // given
        GroupRequestStatusEnum expectedEnum = GroupRequestStatusEnum.values()[0];
        GroupRequestStatus status = Instancio.of(GroupRequestStatus.class)
                .set(field(GroupRequestStatus::getId), expectedEnum.getId())
                .create();

        // when
        GroupRequestStatusEnum result = status.toEnum();

        // then
        assertThat(result).isEqualTo(expectedEnum);
    }

    @Test
    void shouldThrowException_whenIdIsInvalid() {
        // given
        int invalidId = Integer.MAX_VALUE;
        GroupRequestStatus status = Instancio.of(GroupRequestStatus.class)
                .set(field(GroupRequestStatus::getId), invalidId)
                .create();

        // when
        Throwable thrown = catchThrowable(status::toEnum);

        // then
        assertThat(thrown).isInstanceOf(RuntimeException.class);
    }

    @Test
    void shouldCreateGroupRequestStatus_withBuilderAndFields() {
        // given
        String title = "Pending Approval";
        List<GroupRequest> requests = Instancio.createList(GroupRequest.class);

        // when
        GroupRequestStatus status = GroupRequestStatus.builder()
                .title(title)
                .groupRequests(requests)
                .build();

        // then
        assertThat(status).isNotNull();
        assertThat(status.getTitle()).isEqualTo(title);
        assertThat(status.getGroupRequests()).isEqualTo(requests);
        assertThat(status.toString()).contains(title);
    }

    @Test
    void shouldSetAndGetProperties() {
        // given
        GroupRequestStatus status = new GroupRequestStatus();
        String title = "Rejected";
        List<GroupRequest> requests = Instancio.createList(GroupRequest.class);

        // when
        status.setTitle(title);
        status.setGroupRequests(requests);

        // then
        assertThat(status.getTitle()).isEqualTo(title);
        assertThat(status.getGroupRequests()).isEqualTo(requests);
    }
}