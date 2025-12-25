package pl.gamilife.shared.kernel.exception;

import java.io.Serializable;

public interface ErrorCode extends Serializable {
    String getKey();

    String getModule();
}
