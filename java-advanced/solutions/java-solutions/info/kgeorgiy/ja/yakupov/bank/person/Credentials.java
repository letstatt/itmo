package info.kgeorgiy.ja.yakupov.bank.person;

import java.io.Serializable;
import java.util.Objects;

/**
 * Record for person's credentials
 * @param name Name
 * @param surname Surname
 * @param passportId Passport number
 */
public record Credentials(String name, String surname, int passportId) implements Serializable {
    public Credentials(final String name, final String surname, final int passportId) {
        this.name = Objects.requireNonNull(name);
        this.surname = Objects.requireNonNull(surname);
        this.passportId = passportId;
    }
}
